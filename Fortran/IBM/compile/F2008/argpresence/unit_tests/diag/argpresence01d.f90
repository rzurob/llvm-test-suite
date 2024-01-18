!* =================================================================== &
!*
!* DATE                       : March 2, 2011
!*
!* PRIMARY FUNCTIONS TESTED   : Argument Presence Enhancement
!*
!* DESCRIPTION                : Testing proper diagnostic of
!*                              argument presence
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 interface
   subroutine msub1(w,x,y,z)
     integer, optional :: w(:), x(:), y(:), z(:)
   end subroutine
   integer function mfunc1(w,x,y,z)
     integer, optional :: w(:), x(:), y(:), z(:)
   end function
 end interface

 integer, pointer :: p1(:), p2(:)
 integer, target :: t1(2)
 integer, allocatable :: a1(:), a2(:)

 p2 => t1
 allocate (a2(1))
 nullify(p1)

 call msub1(p1,p2,a1,a2)
 t1 = mfunc1(p1,p2,a1,a2)
 call msub2(p1,p2,a1,a2)
 t1 = mfunc2(p1,p2,a1,a2)

 end
 subroutine msub1(w,x,y,z)
   integer, optional :: w(:), x(:), y(:), z(:)
   print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
 end subroutine

 integer function mfunc1(w,x,y,z)
   integer, optional :: w(:), x(:), y(:), z(:)
   mfunc1 = 1
   print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
 end function
 subroutine msub2(w,x,y,z)
   integer, optional :: w(:), x(:), y(:), z(:)
   print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
 end subroutine

 integer function mfunc2(w,x,y,z)
   integer, optional :: w(:), x(:), y(:), z(:)
   mfunc2 = 1
   print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
 end function
