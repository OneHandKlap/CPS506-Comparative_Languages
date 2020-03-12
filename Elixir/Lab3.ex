defmodule Lab3 do
    def firstTwo(list) do
        (hd list) == (hd(tl list))
    end
    def listIsEven(list) do
        rem(length(list),2)==0
    end
    def frontBack(list) do
        tl(list)++hd(list)
    end
    def nextNineNine(list) do
        [hd(list)]++([99]++tl(list))
    end
    def sayHello (list) do
        length(list--["hello"])!=length(list)
    end
    def isCoord(list) do
        length(list)==2 &&
            is_number(hd(list))&&
            is_number(hd(tl(list)))
    end
end