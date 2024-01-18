!empty CONTAINS within module subprogram
    module foo
    contains
       subroutine foo2()
           print *, 'Hello world!'
           contains
       end subroutine foo2
    end module foo
