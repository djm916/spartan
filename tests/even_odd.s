def even : fun n => if = 0 n then true
                    else odd (- 1 n)
                    end
           end

def  odd : fun n => if = 0 n then false
                    else even (- 1 n)
                    end
           end

def main : even 1001
