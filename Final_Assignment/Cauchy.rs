use std::fmt;
use std::ops;
use std::cmp;

pub struct CauchyList {
    pub p: i32,
    pub content: Vec<i32>
}

impl CauchyList {
  fn new(p: i32, content: Vec<i32>) -> CauchyList{
    return CauchyList { p: p, content: content};
  }
}

impl cmp::PartialEq for CauchyList {
    fn eq(&self, operand: &Self) -> bool {    
        if self.content.len() != operand.content.len() || self.p != operand.p {
            false;
        }
        for(i, j) in self.content.iter().enumerate() {
            if *j != operand.content[i]  {
            false;
            }
        }
        
        return true;
    }
}

impl ops::Add<CauchyList> for CauchyList {
    type Output = CauchyList;
    fn add(self, operand: CauchyList) -> CauchyList {
        let mut result = CauchyList::new(self.p, vec!());
        if self.content.len() >= operand.content.len() {
            for(i, v) in self.content.iter().enumerate() {
                if operand.content.len() <= i {
                    result.content.push((*v)%self.p);
                } else {
                    result.content.push((*v + operand.content[i])%self.p);
                }
            }
        } else {
            for(i, v) in operand.content.iter().enumerate() {
                if self.content.len() <= i {
                    result.content.push((*v)%self.p);
                } else {
                    result.content.push((*v + self.content[i])%self.p);
                }
            }
        }
        return result;
    }
}

impl ops::Sub<CauchyList> for CauchyList {
    type Output = CauchyList;
    fn sub(self, operand: CauchyList) -> CauchyList {
        let mut result = CauchyList::new(self.p, vec!());
        if self.content.len() >= operand.content.len() {
            for(i, v) in self.content.iter().enumerate() {
                if operand.content.len() <= i {
                    result.content.push((*v)%self.p);
                } else {
                    result.content.push((*v - operand.content[i])%self.p);
                }
            }
        } else {
            for(i, v) in operand.content.iter().enumerate() {
                if self.content.len() <= i {
                    result.content.push((-*v)%self.p);
                } else {
                    result.content.push((self.content[i]-*v)%self.p);
                }
            }
        }
        return result;
    }
}


impl ops::Mul<CauchyList> for CauchyList {
    type Output = CauchyList;
    fn mul(self, operand: CauchyList) -> CauchyList {
        let mut result = CauchyList::new(self.p, vec!());
        for i in 0..(self.content.len() + operand.content.len() - 1) {
            let mut val = 0;
            for j in 0..i+1 {
                let a = if j>self.content.len()-1 { 0 } else { self.content[j] };
                let b = if i-j>operand.content.len()-1 { 0 } else { operand.content[i-j] };
                val+=a*b;
            }
            result.content.push(val%self.p);
        }
        return result;
    }
}

impl ops::Mul<i32> for CauchyList {
    type Output = CauchyList;
    
    fn mul(self, operand: i32) -> CauchyList {
        let mut result = CauchyList::new(self.p, vec!());
        for(_i, j) in self.content.iter().enumerate() {
            result.content.push(*j * operand);
        }
        return result;
    }
}


impl fmt::Display for CauchyList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {    
        write!(f, "P: { }\nLength: { }\nContent: {:?}\n", self.p, self.content.len(), self.content)
    }
}


fn main() {
    let a = CauchyList::new(31, vec![17, 9, 22, 27, 28, 27, 15, 28, 24, 1]);
    let b = CauchyList::new(31, vec![12, 4, 7, 15, 13, 4]);
    print!("{ }", (a*b));
    //print!("{:?}", CauchyList { p: 5, content: vec!()});
}