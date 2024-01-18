!* =================================================================== &
!*
!* DATE                       : March 2, 2011
!* ORIGIN                     : AIX Compiler Development,
!*
!* PRIMARY FUNCTIONS TESTED   : Argument Presence Enhancement
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              argument presence
!*                              argument keywords
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 integer, pointer :: p1(:), p2(:)
 integer, target :: t1(2)
 integer, allocatable :: a1(:), a2(:)

 p2 => t1
 allocate (a2(1))
 nullify(p1)

 call sub1(p1,p2,a1,a2)
 call sub1(z=p1,y=p2,x=a1,w=a2)
 call sub1(w=p1)
 call sub1(x=p1)
 call sub1(y=p1)
 call sub1(z=p1)
 call sub1(y=p1,w=a1)
 call sub1(z=p1,x=p2)
 t1 = func1(p1,p2,a1,a2)
 t1 = func1(z=p1,y=p2,x=a1,w=a2)
 t1 = func1(w=p1)
 t1 = func1(x=p1)
 t1 = func1(y=p1)
 t1 = func1(z=p1)
 t1 = func1(y=p1,w=a1)
 t1 = func1(z=p1,x=p2)

 contains
 subroutine sub1(w,x,y,z)
   integer, optional :: w(:), x(:), y(:), z(:)
   print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
 end subroutine

 integer function func1(w,x,y,z)
   integer, optional :: w(:), x(:), y(:), z(:)
   func1 = 1
   print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)
 end function
 end
