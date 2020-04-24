use std::fmt;
use std::ops;
use std::cmp;
use std::cmp::Ordering;

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
    fn eq(&self, other: &Self)->bool{
        if (self.p==other.p)&&(self.content.len()==other.content.len()){
            for i in (0..self.content.len()){
                if(self.content[i]!=other.content[i]){
                    return false;
                }
            }
            return true;
        }
        else{
            return false;
        }
    } 
}
impl cmp::Ord for CauchyList{
    fn cmp(&self, other:&Self)->Ordering{
        Some(self.content.len().cmp(&other.content.len()))
    }
}
impl cmp::PartialOrd for CauchyList{
    fn partial_cmp(&self, other:&Self)->Option<Ordering>{
        Some(self.cmp(other))
    }
}
impl cmp::Eq for CauchyList{}

    

impl fmt::Display for CauchyList{
    fn fmt(&self, f: &mut fmt::Formatter)->fmt::Result{
        write!(f, "P: {}\nLength: {}\nContent: {:?}",self.p,self.content.len(),self.content)
    }
}
// impl ops::Add<CauchyList> for vec!{
//     fn add(&self, other: CauchyList)-> CauchyList{
//         let mut result=vec![];
//         for i in (0..(cmp::max(self.content.len(),other.content.len()))){
//             if i < cmp::min(self.content.len(),other.content.len()){
//                 result.append(self.content[i]+other.content[i]);
//             }
//             else{
//                 result.append(0);
//             }
//         }
//         return result;
//     }
// }

fn main(){
    let c1= CauchyList::new(31, vec![1,2,3,4,5]);
    let c2= CauchyList::new(31, vec![1,3,3,4,5]);
    println!("{}",c1==c2);
    println!("{}",c1);
}