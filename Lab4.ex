defmodule Lab4 do
    def sumEven([],sum), do: sum
    def sumEven(list, acc) do
        if is_integer(hd(list)) && rem(hd(list),2)==0 do
            sumEven(tl(list),acc+hd(list))
        else
            sumEven(tl(list),acc)
        end
    end
    def sumNum([],acc),do: acc
    def sumNum(list,acc) do
        if is_number(hd(list)) do
            sumNum(tl(list),acc+hd(list))
        else
            sumNum(tl(list),acc)
        end
    end
    def tailFib(0,_acc),do: 0
    def tailFib(1,_acc),do: 1
    def tailFib(num,acc) do
        num<1 && :error ||
        acc+tailFib(num-1,acc)+tailFib(num-2,acc)
    end
end
            
