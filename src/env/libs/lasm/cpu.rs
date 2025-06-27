use std::collections::HashMap;

// This file is a part of the LASM (Lucia Assembly) module.
// It provides a simple CPU simulator that can execute a limited set of assembly-like instructions.
// Lucia version 2.0.0, module: lasm@1.0.3

#[derive(Default)]
pub struct CPU {
    regs: HashMap<String, i32>,
    memory: HashMap<u32, u8>,
    pc: usize,
    program: Vec<String>,
    zero: bool,
    negative: bool,
    labels: HashMap<String, usize>,

    config: HashMap<String, String>,
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            regs: HashMap::new(),
            memory: HashMap::new(),
            pc: 0,
            program: Vec::new(),
            zero: false,
            negative: false,
            labels: HashMap::new(),
            config: HashMap::new(),
        }
    }

    pub fn load_program(&mut self, asm: String) -> Result<(), (String, String)> {
        if asm.trim().is_empty() {
            return Err(("RuntimeError".to_string(), "Program is empty".to_string()));
        }
    
        self.pc = 0;
        self.regs.clear();
        self.memory.clear();
        self.zero = false;
        self.negative = false;
        self.config.clear();
        self.labels.clear();
    
        let mut lines = asm.lines();
        let mut config_lines = Vec::new();
        let mut program_lines = Vec::new();
    
        for line in &mut lines {
            let trimmed = line.trim();
            if trimmed.starts_with("#[") && trimmed.ends_with(']') {
                config_lines.push(trimmed.to_string());
            } else if !trimmed.is_empty() {
                program_lines.push(trimmed.to_string());
                program_lines.extend(lines.map(|l| l.trim().to_string()));
                break;
            }
        }
    
        for cfg in config_lines {
            if let Some((key, val)) = Self::parse_attribute(&cfg)? {
                self.config.insert(key, val);
            }
        }
    
        let comment_prefix = self.config.get("comment").map(String::as_str).unwrap_or("");
        let filtered_program: Vec<String> = if !comment_prefix.is_empty() {
            program_lines
                .into_iter()
                .map(|line| {
                    if let Some(pos) = line.find(comment_prefix) {
                        line[..pos].trim_end().to_string()
                    } else {
                        line
                    }
                })
                .filter(|line| !line.is_empty())
                .collect()
        } else {
            program_lines
        };        
    
        let mut program_without_labels = Vec::new();
        for (idx, line) in filtered_program.iter().enumerate() {
            if line.ends_with(':') {
                let label_name = line.trim_end_matches(':').to_string();
                self.labels.insert(label_name, program_without_labels.len());
            } else {
                program_without_labels.push(line.clone());
            }
        }
    
        self.program = program_without_labels;
    
        Ok(())
    }
    

    fn parse_attribute(line: &str) -> Result<Option<(String, String)>, (String, String)> {
        if line.starts_with("#[") && line.ends_with(']') {
            let inner = &line[2..line.len()-1];
            if let Some(eq_pos) = inner.find('=') {
                let key = inner[..eq_pos].trim().to_string();
                let val = inner[eq_pos+1..].trim().to_string();
                return Ok(Some((key, val)));
            }
            return Ok(None);
        }
        Ok(None)
    }    

    fn strip_reg_prefix<'a>(&self, reg: &'a str) -> &'a str {
        if let Some(prefix) = self.config.get("register_prefix") {
            if !prefix.is_empty() && reg.starts_with(prefix) {
                &reg[prefix.len()..]
            } else {
                reg
            }
        } else {
            reg
        }
    }

    fn get_val(&self, operand: &str) -> Result<i32, (String, String)> {
        if let Ok(num) = operand.parse::<i32>() {
            Ok(num)
        } else {
            let reg_name = self.strip_reg_prefix(operand);
            self.regs.get(reg_name)
                .copied()
                .ok_or(("RuntimeError".to_string(), format!("Unknown register '{}'", operand)))
        }
    }

    fn set_flags(&mut self, val: i32) {
        self.zero = val == 0;
        self.negative = val < 0;
    }

    pub fn step(&mut self) -> Result<Option<i32>, (String, String)> {
        if self.pc >= self.program.len() {
            return Ok(None);
        }
        let line = &self.program[self.pc];
        self.pc += 1;

        if line.is_empty() {
            return Ok(None);
        }

        let mut parts = line.splitn(2, ' ');
        let opcode = parts.next().unwrap();
        let args_str = parts.next().unwrap_or("").trim();

        let args: Vec<_> = if args_str.is_empty() {
            Vec::new()
        } else {
            args_str.split(',').map(|s| s.trim()).collect()
        };

        match (opcode, args.as_slice()) {
            ("mov", [dst, src]) => {
                let dst = self.strip_reg_prefix(dst);
                let val = self.get_val(src)?;
                self.regs.insert(dst.to_string(), val);
                self.set_flags(val);
                Ok(None)
            }
            ("add", [dst, src]) => {
                let dst = self.strip_reg_prefix(dst);
                let a = self.regs.get(dst).copied().unwrap_or(0);
                let b = self.get_val(src)?;
                let res = a.wrapping_add(b);
                self.regs.insert(dst.to_string(), res);
                self.set_flags(res);
                Ok(None)
            }
            ("sub", [dst, src]) => {
                let dst = self.strip_reg_prefix(dst);
                let a = self.regs.get(dst).copied().unwrap_or(0);
                let b = self.get_val(src)?;
                let res = a.wrapping_sub(b);
                self.regs.insert(dst.to_string(), res);
                self.set_flags(res);
                Ok(None)
            }
            ("mul", [dst, src]) => {
                let dst = self.strip_reg_prefix(dst);
                let a = self.regs.get(dst).copied().unwrap_or(0);
                let b = self.get_val(src)?;
                let res = a.wrapping_mul(b);
                self.regs.insert(dst.to_string(), res);
                self.set_flags(res);
                Ok(None)
            }
            ("ldr", [dst, addr]) => {
                let dst = self.strip_reg_prefix(dst);
                let addr_val = addr.strip_prefix('[')
                    .and_then(|s| s.strip_suffix(']'))
                    .ok_or(("RuntimeError".to_string(), "Invalid memory address syntax in ldr".to_string()))?;
                let addr_num = addr_val.parse::<u32>()
                    .map_err(|_| ("RuntimeError".to_string(), "Invalid memory address number in ldr".to_string()))?;
                let val = *self.memory.get(&addr_num).unwrap_or(&0);
                self.regs.insert(dst.to_string(), val as i32);
                self.set_flags(val as i32);
                Ok(None)
            }
            ("str", [src, addr]) => {
                let src = self.strip_reg_prefix(src);
                let addr_val = addr.strip_prefix('[')
                    .and_then(|s| s.strip_suffix(']'))
                    .ok_or(("RuntimeError".to_string(), "Invalid memory address syntax in str".to_string()))?;
                let addr_num = addr_val.parse::<u32>()
                    .map_err(|_| ("RuntimeError".to_string(), "Invalid memory address number in str".to_string()))?;
                let val = self.regs.get(src).copied().unwrap_or(0);
                self.memory.insert(addr_num, val as u8);
                Ok(None)
            }
            ("cmp", [op1, op2]) => {
                let a = self.get_val(op1)?;
                let b = self.get_val(op2)?;
                let res = a.wrapping_sub(b);
                self.set_flags(res);
                Ok(None)
            }
            ("b", [label]) => {
                let target = if let Ok(num) = label.parse::<usize>() {
                    num
                } else {
                    *self.labels.get(&label[..]).ok_or(("RuntimeError".to_string(), format!("Unknown label '{}'", label)))?
                };                
            
                if target >= self.program.len() {
                    return Err(("RuntimeError".to_string(), "Branch target out of range".to_string()));
                }
                self.pc = target;
                Ok(None)
            }            
            ("beq", [label]) => {
                if self.zero {
                    let target = self.get_label_target(label)?;
                    if target >= self.program.len() {
                        return Err(("RuntimeError".to_string(), "Branch target out of range".to_string()));
                    }
                    self.pc = target;
                }
                Ok(None)
            }
            ("bne", [label]) => {
                if !self.zero {
                    let target = self.get_label_target(label)?;
                    if target >= self.program.len() {
                        return Err(("RuntimeError".to_string(), "Branch target out of range".to_string()));
                    }
                    self.pc = target;
                }
                Ok(None)
            }            
            ("jmp", [label]) => {
                let target = self.get_label_target(label)?;
                self.pc = target;
                Ok(None)
            }
            ("je", [label]) => {
                if self.zero {
                    let target = self.get_label_target(label)?;
                    self.pc = target;
                }
                Ok(None)
            }
            ("jne", [label]) => {
                if !self.zero {
                    let target = self.get_label_target(label)?;
                    self.pc = target;
                }
                Ok(None)
            }
            ("syscall", []) => {
                let syscall_num = self.regs.get("rax").copied().unwrap_or(0);
                let arg_regs = ["rdi", "rsi", "rdx", "r10", "r8", "r9"];
                let mut args = Vec::new();
                for &reg in arg_regs.iter() {
                    args.push(self.regs.get(reg).copied().unwrap_or(0));
                }
                match syscall_num {
                    0 => {
                        if let Some(code) = self.regs.get("rdi") {
                            return Ok(Some(*code));
                        }
                        Ok(Some(0))
                    }
                    1 => {
                        if let Some(code) = self.regs.get("rdi") {
                            return Err(("ProcessExit".to_string(), code.to_string()));
                        }
                        Err(("ProcessExit".to_string(), 0.to_string()))
                    }
                    2 => {
                        let fd = self.regs.get("rdi").copied().unwrap_or(1);
                        let addr = self.regs.get("rsi").copied().unwrap_or(0);
                        let len = self.regs.get("rdx").copied().unwrap_or(0);
                    
                        if fd != 1 && fd != 2 {
                            return Err(("RuntimeError".to_string(), format!("Unsupported file descriptor: {}", fd)));
                        }
                    
                        let mut output = String::new();
                        for i in 0..len {
                            let byte = *self.memory.get(&(addr as u32 + i as u32)).unwrap_or(&0);
                            output.push(byte as char);
                        }
                        
                        match fd {
                            1 => print!("{}", output),
                            2 => eprint!("{}", output),
                            _ => return Err((
                                "RuntimeError".to_string(),
                                format!("Unsupported file descriptor: {}", fd),
                            )),
                        }
                        Ok(None)
                    }
                    3 => {
                        let fd = self.regs.get("rdi").copied().unwrap_or(1);
                        let addr = self.regs.get("rsi").copied().unwrap_or(0);
                    
                        let msg_str = self.read_mem_string(addr as u64).map_err(|_| (
                            "RuntimeError".to_string(),
                            "Invalid UTF-8 string in syscall".to_string(),
                        ))?;
                    
                        match fd {
                            1 => print!("{}", msg_str),
                            2 => eprint!("{}", msg_str),
                            _ => return Err((
                                "RuntimeError".to_string(),
                                format!("Unsupported file descriptor: {}", fd),
                            )),
                        }
                    
                        Ok(None)
                    }
                    4 => {
                        let ms = self.regs.get("rdi").copied().unwrap_or(0);
                        std::thread::sleep(std::time::Duration::from_millis(ms as u64));
                        Ok(None)
                    }                    
                    _ => Err(("RuntimeError".to_string(), format!("Unknown syscall number {}", syscall_num))),
                }
            }
            ("ret", [reg]) => {
                let reg = self.strip_reg_prefix(reg);
                let val = *self.regs.get(reg).ok_or(("RuntimeError".to_string(), format!("Unknown register '{}'", reg)))?;
                Ok(Some(val))
            }
            _ => Err(("RuntimeError".to_string(), format!("Unknown or malformed instruction '{}'", line))),
        }
    }

    fn read_mem_string(&self, addr: u64) -> Result<String, ()> {
        let mut bytes = Vec::new();
        let mut current = addr;
        loop {
            let byte = *self.memory.get(&(current as u32)).ok_or(())?;
            if byte == 0 { break; }
            bytes.push(byte as u8);
            current += 1;
        }
        std::str::from_utf8(&bytes).map(|s| s.to_string()).map_err(|_| ())
    }

    fn get_label_target(&self, label: &str) -> Result<usize, (String, String)> {
        self.labels.get(label)
            .copied()
            .ok_or(("RuntimeError".to_string(), format!("Unknown label '{}'", label)))
    }

    pub fn run(&mut self) -> Result<Option<i32>, (String, String)> {
        while self.pc < self.program.len() {
            if let Some(val) = self.step()? {
                return Ok(Some(val));
            }
        }
        Ok(None)
    }
}
