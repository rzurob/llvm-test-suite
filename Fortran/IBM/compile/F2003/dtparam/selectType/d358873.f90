    TYPE Base (n)
        INTEGER, LEN :: n

        TYPE (Base(n)), POINTER :: cmp
        INTEGER :: my_arr(n) = -1    
    END TYPE

    CLASS(Base(:)), POINTER :: ptr   
    TYPE(Base(10)), TARGET :: tgt

    ptr => tgt

    ALLOCATE (ptr%cmp)            

END
