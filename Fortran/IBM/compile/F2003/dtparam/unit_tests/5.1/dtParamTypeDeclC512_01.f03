       ! C512: If the POINTER attribute is specified, the ALLOCATABLE,
       ! TARGET, EXTERNAL, and INTRINSIC attributes must not be specified.
       type :: dt(k,l)
         integer, kind :: k
         integer, len :: l
         integer(k) i(l)
       end type

       class(dt(2,:)), pointer :: x
       allocatable :: x
       external :: x
       intrinsic :: x

       class(dt(4,:)), pointer :: y
       target :: y
       end
