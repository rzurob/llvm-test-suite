!* =================================================================== &
!* XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* =================================================================== &
!*
!* TEST CASE TITLE            : argpresence04f.f
!*
!* PROGRAMMER                 : David Nichols
!* DATE                       : March 2, 2011
!* ORIGIN                     : AIX Compiler Development,
!*                            : IBM Software Solutions Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : Argument Presence Enhancement
!*
!* DRIVER STANZA              : xlf2008
!*
!* DESCRIPTION                : Testing proper functionality of
!*                              argument presence 
!*                              as internal procedures
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 integer, pointer :: p1(:), p2(:)
 integer, target :: t1(2)
 integer, allocatable :: a1(:), a2(:) 

 p2 => t1
 allocate (a2(1))
 nullify(p1)

 call sub1(p1,p2,a1,a2) 
 t1 = func1(p1,p2,a1,a2)

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
