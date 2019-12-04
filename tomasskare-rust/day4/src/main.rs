fn main() {
    let range_start: u32 = 153517;
    let range_end: u32 = 630395;
    let factors: [u32; 6] = [100000, 10000, 1000, 100, 10, 1];

    let mut num1 = 0;
    let mut num2 = 0;

    let mut i = range_start;
    while i <= range_end {
        let mut nums: [u32; 6] = [0; 6];

        let mut i2 = i;
        for d in 0..6 {
            let i3 = i2 / factors[d];
            nums[d] = i3;
            i2 -= i3 * factors[d];
        }

        i += 1;

        let mut last: u32 = 0;
        let mut num_last = 0;

        let mut ok1 = 0;
        let mut ok2 = 0;
        for d in 0..6 {
            if d < 5 && nums[d + 1] < nums[d] {
                // Skip over all values that are also decreasing.
                nums[d + 1] = nums[d];
                for d2 in d + 2..6 {
                    nums[d2] = 0;
                }
                let mut i2 = 0;
                for d2 in 0..6 {
                    i2 += nums[d2] * factors[d2];
                }
                i = i2;

                ok1 = 0;
                ok2 = 0;
                break;
            }

            if d < 5 && nums[d] == nums[d + 1] {
                ok1 = 1;
            }

            if last == nums[d] {
                num_last += 1;
            } else {
                if num_last == 2 {
                    ok2 = 1;
                }
                last = nums[d];
                num_last = 1;
            }
            if d == 5 && num_last == 2 {
                ok2 = 1;
            }
        }

        num1 += ok1;
        num2 += ok2;
    }

    println!("Part 1: {}", num1);
    println!("Part 2: {}", num2);
}
