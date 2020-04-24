fn main(){
    let x:  u8= 7;
    println!("x: {}",x);
    let r = 3 as f64/4 as f64;
    println!("r: {}",r);
    let tup = (42, 3.14, '!');
    let (x,y,z)=tup;
    println!("x: {}; y: {}; z: {}",x,y,z);
    println!("{}",tup.0);

    let array = [1,2,3,4,5];
    println!("{}, {}, {}",array[0],array[1],array[4]);
    print_hello();
    print_val(7);


    let state = get_temp(33);
    println!("{}",state);

    let nums = [1,2,3,4,5,6,7,8];
    for num in nums.iter(){
        print!("{}",num);
    }
    for i in (0..10).rev(){
        print!("{}",i);
    }
    let tail = &nums[4..8];
    println!("{}",tail[0]);
}

fn get_temp(temp:i32) -> &'static str{
    if temp<0{
        "SHIT IS FUCKING COLD"
    }
    else if temp<32{
        "SHIT AINT THAT WARM"
    }
    else{
        "SHIT IS FUCKING HAAAAWWWT"
    }

}
fn print_hello(){
    println!("Hello!");
}
fn print_val(n:i32){
    println!("{}",n);
}
