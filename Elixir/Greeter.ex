defmodule Greeter do
    def hello(name) do
        "Hello, " <> name
    end
    def hello() do
        "Hello there!"
    end
    def hello(name1, name2) do
        "Hello " <> name1 <>" and "<> name2
    end
end