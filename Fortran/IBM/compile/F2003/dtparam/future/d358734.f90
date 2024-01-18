TYPE Base (k)
   integer, KIND :: k
END TYPE

CLASS (Base(4)), POINTER :: poly
TYPE (Base(4)), TARGET :: tgt

poly => tgt

select type (poly)
        class is (Base(4))
           print *, 1
        class is (Base(8))  !<-- Should be accepted
           print *, 2
end select
END
