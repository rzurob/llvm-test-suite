!* =================================================================== &
!* XL Fortran Test Case                          IBM INTERNAL USE ONLY
!* =================================================================== &
!*
!* TEST CASE TITLE            : argpresence01f.f
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
!*                              as module procedures
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module argpresence01f_mod

 integer, pointer :: mp1(:), mp2(:)
 integer, target :: mt1(2)
 integer, allocatable :: ma1(:), ma2(:) 

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

 subroutine mcallfunc1(mxp1,mxa1)
   integer, pointer :: mxp1(:)
   integer, allocatable :: mxa1(:)

   integer, target :: mxt1(2)

   mxp1 => mxt1
   allocate (mxa1(1))
   mxt1 = mfunc1(mxp1,mxa1)
   nullify(mxp1)
   deallocate (mxa1)
   mxt1 = mfunc1(mxp1,mxa1)
 end subroutine
 end module 
 use argpresence01f_mod

 integer, pointer :: p1(:), p2(:)
 integer, target :: t1(2)
 integer, allocatable :: a1(:), a2(:) 

 p2 => t1
 allocate (a2(1))
 nullify(p1)

 call msub1(p1,p2,a1,a2) 
 t1 = mfunc1(p1,p2,a1,a2)
 call mcallfunc1(p1,a1)
 end
