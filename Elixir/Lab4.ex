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
    def tailFib(num),do: tailFib(num,1,1)
    def tailFib(1,_,acc),do: acc
    def tailFib(num,curr,acc) do
        num<=0 && :error ||
        tailFib(num-1,curr+acc,curr)
    end
end
            
