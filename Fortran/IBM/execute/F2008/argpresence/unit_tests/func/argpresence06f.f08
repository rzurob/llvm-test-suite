!* =================================================================== &
!*
!* DATE                       : March 2, 2011
!*
!* PRIMARY FUNCTIONS TESTED   : Argument Presence Enhancement
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              argument presence
!*                              as nest procedure calls
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 integer, pointer :: p1(:), p2(:)
 integer, target :: t1(2)
 integer, allocatable :: a1(:), a2(:)

 p2 => t1
 allocate (a2(1))
 nullify(p1)

 call msub1(p1,p2,a1,a2)
 t1 = mfunc1(p1,p2,a1,a2)
 call mcallfunc1(p1,a1)
 call mcall2func1(p1,p2,a1,a2)

 contains
 subroutine msub1(w,x,y,z)
   integer, optional :: w(:), x(:), y(:), z(:)
   print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
 end subroutine

 integer function mfunc1(w,x,y,z)
   integer, optional :: w(:), x(:), y(:), z(:)
   mfunc1 = 1
   print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
 end function

 subroutine mcallfunc1(p1,a1)
   integer, pointer :: p1(:)
   integer, allocatable :: a1(:)

   integer, target :: t1(2)

   p1 => t1
   allocate (a1(1))
   t1 = mfunc1(p1,a1)
   nullify(p1)
   deallocate (a1)
   t1 = mfunc1(p1,a1)
 end subroutine
 subroutine mcall2func1(p1,p2,a1,a2)
   integer, pointer :: p1(:), p2(:)
   integer, allocatable :: a1(:), a2(:)

   t1 = mfunc1(p1,p2,a1,a2)
   call msub1(p1,p2,a1,a2)
 end subroutine
 end
