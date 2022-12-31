// Saishnu Ramesh Kumar (300758706)
// CSCI 117 - Lab 11

local SumListS SumList Out1 Out2 SumList2 in
    fun{SumList L} //Declarative iterative 
        case L of nil then 0
        []'|'(1:H 2:T) then (H+ {SumList T})
        end
    end

    fun{SumListS L} //Stateful iterative
    C = newCell 0
    {SumList2 C L}
    end 
    
    fun{SumList2 C L}
        case L of nil then @C
        []'|(1:H 2:T) then C :=(H + @C) {SumList2 C T}
        end 
    end 

        Out1 = {SumList[1 2 3 4]}
        Out2 = {SumListS [1 2 3 4]}
        skip Browse Out1 
        skip Browse Out2 
        skip Full

end 

local foldL2 foldL Out1 Out2 Z in 
    fun{foldL F Z L}
        case L of nil then Z 
        [] '|' (1:H 2:T) then 
            {foldL F {F Z H} T}
        end 
    end 

    fun{foldL2 F Z L}{
        FL2 = newCell 0
        foldL3 in 
        proc {foldL3 F Z L}
        case L of nil then FL2 := @FL2
        [] '|'(1:H 2:T) then 
        FLS := 0
        FLS := {F Z H}

        {foldL3 F @FLS T}
        end 
    end 

        {foldL3 F Z L}
        @FLS
    end 

        Out1 = {foldL fun P {$ X Y} (X + Y) end 3 [1 2 3 4]}
        Out2 = {foldL2 fun {$X Y} (X+Y) end 3 [1 2 3 4]}
        skip Browse Out1
        skip Browse Out2 
        skip Full 
    }
