!* =================================================================== &
!*
!* DATE                       : March 2, 2011
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : Argument Presence Enhancement
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              argument presence
!*                              NULL() intrinsic
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 type baser(i)
   integer, len :: i
   integer :: j(i)
 end type
 type base(k,l,j)
   integer, kind :: k
   integer, len :: l,j
   integer(k) :: i
   character(l), pointer :: c(:)
   type(baser(j)), allocatable :: sub
 end type

 type(base(4,:,:)), pointer     :: p1(:), p2(:)
 type(base(2,:,:)), allocatable :: a1(:), a2(:)
 type(base(4,5,5)), target      :: t1(2)

 t1 = base(4,5,5)(1,NULL(),baser(5)(1))

 p2 => t1
 allocate (a2(1), source = base(2,5,5)(1,NULL(),baser(5)(1)))
 nullify(p1)

 call sub1(NULL(p1),NULL(p2),NULL(a1),NULL(a2))
 t1%i = func1(NULL(p1),NULL(p2),NULL(a1),NULL(a2))

 contains
 subroutine sub1(w,x,y,z)
 type(base(4,*,*)), optional :: w(:), x(:)
 type(base(2,*,*)), optional :: y(:), z(:)

 print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)

 end subroutine
 integer function func1(w,x,y,z)
 type(base(4,*,*)), optional :: w(:), x(:)
 type(base(2,*,*)), optional :: y(:), z(:)
 func1 = 1

 print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)

 end function
 end
