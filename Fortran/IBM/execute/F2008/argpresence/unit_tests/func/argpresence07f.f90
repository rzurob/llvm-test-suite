!* =================================================================== &
!* XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* =================================================================== &
!*
!* TEST CASE TITLE            : argpresence07f.f
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
!*                              with derived types
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 type baser
   integer :: j
 end type
 type base
   integer :: i
   character, pointer :: c(:)
   type(baser), allocatable :: sub
 end type

 type(base), pointer     :: p1(:), p2(:)
 type(base), allocatable :: a1(:), a2(:)
 type(base), target      :: t1(2)

 t1 = base(1,NULL(),baser(1))

 p2 => t1
 allocate (a2(1))
 nullify(p1)

 call sub1(p1,p2,a1,a2)
 t1%i = func1(p1,p2,a1,a2)

 contains
 subroutine sub1(w,x,y,z)
 type(base), optional :: w(:), x(:), y(:), z(:)

 print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z) 

 end subroutine
 integer function func1(w,x,y,z)
 type(base), optional :: w(:), x(:), y(:), z(:)
 func1 = 1

 print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z) 

 end function
 end
