!* =================================================================== &
!* XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* =================================================================== &
!*
!* TEST CASE TITLE            : argpresence11f.f
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
!*                              components as allocatables/pointers
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
   type(baser(j)), allocatable :: sub(:)
 end type

 type(base(4,:,:)), pointer     :: p1(:), p2(:)
 type(base(2,:,:)), allocatable :: a1(:), a2(:)
 type(base(4,5,5)), target      :: t1(2), t2(2)

 t1 = base(4,5,5)(1,NULL(),[baser(5)(1)])
 t2 = base(4,5,5)(1,NULL(),[baser(5)(1)])
 p2 => t2
 p1 => t1
 allocate (a1(1), source = base(2,5,5)(1,NULL(),[baser(5)(1)]))
 allocate (a2(1), source = base(2,5,5)(1,NULL(),[baser(5)(1)]))

 nullify(p1(1)%c)
 allocate(p2(1)%c(5))
 deallocate(a1(1)%sub)

 call sub1(p1(1)%c,p2(1)%c,a1(1)%sub,a2(1)%sub)
 t1%i = func1(p1(1)%c,p2(1)%c,a1(1)%sub,a2(1)%sub)

 contains
 subroutine sub1(w,x,y,z)
 character(*), optional :: w(:), x(:)
 type(baser(5)), optional :: y(:), z(:)

 print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)

 end subroutine
 integer function func1(w,x,y,z)
 character(*), optional :: w(:), x(:)
 type(baser(5)), optional :: y(:), z(:)
 func1 = 1

 print *, PRESENT(w), PRESENT(x), PRESENT(y), PRESENT(z)

 end function
 end
