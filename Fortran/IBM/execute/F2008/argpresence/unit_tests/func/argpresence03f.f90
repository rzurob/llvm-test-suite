!* =================================================================== &
!*
!* DATE                       : March 2, 2011
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : Argument Presence Enhancement
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              argument presence
!*                              as external procedures
!*
!234567890123456789012345678901234567890123456789012345678901234567890



 integer, pointer :: p1(:), p2(:)
 integer, target :: t1(2)
 integer, allocatable :: a1(:), a2(:)

 interface
   subroutine exsub1(w,x,y,z)
   integer, optional :: w(:), x(:), y(:), z(:)
   end subroutine
   integer function exfunc1(w,x,y,z)
   integer, optional :: w(:), x(:), y(:), z(:)
   end function
 end interface

 p2 => t1
 allocate (a2(1))
 nullify(p1)


 call exsub1(p1,p2,a1,a2)
 t1 = exfunc1(p1,p2,a1,a2)

 end
 subroutine exsub1(w,x,y,z)
 integer, optional :: w(:), x(:), y(:), z(:)
 print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
 end subroutine
 function exfunc1(w,x,y,z)
 integer, optional :: w(:), x(:), y(:), z(:)
 integer :: exfunc1
 exfunc1 = 1
 print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
 end function

