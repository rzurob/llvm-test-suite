! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/genericName/functional/genericGenericNameScalar016.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : generic-name: scalar derived type calling
!*                                             generic-binding name same as generic-interface name
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure, pass :: plusplus
         procedure, pass :: plusplusconst
         generic :: pp => plusplus, plusplusconst
   end type

   type, extends(base) :: child    ! (20,4)
      real(k1) :: j
      contains
         procedure, pass :: plusplus => pluspluschild
         procedure, pass :: plusplustwoconst
         generic :: pp => plusplustwoconst
   end type

   interface pp
      module procedure plusplus
      module procedure plusplusconst
      module procedure plusplustwoconst
   end interface

   contains

   subroutine plusplus(a)
      class(base(*,4)), intent(inout) :: a

      a%i = a%i + 1

   end subroutine

   subroutine plusplusconst(a, i)
      class(base(*,4)), intent(inout) :: a
      integer, intent(in) :: i

      a%i = a%i + i
      select type ( a )
         type is ( child(*,4) )
            a%j = a%j + real(i)
      end select

   end subroutine

   subroutine plusplustwoconst(a, i, j)
      class(child(*,4)), intent(inout) :: a
      integer, intent(in) :: i
      real, intent(in) :: j

      a%i = a%i + i
      a%j = a%j + j

   end subroutine

   subroutine pluspluschild(a)
      class(child(*,4)), intent(inout) :: a

      call a%base%pp()

      a%j = a%j + 1.0

   end subroutine

end module


program genericGenericNameScalar016
   use m

   type(base(20,4)) :: b1
   class(base(:,4)), pointer :: b2

   type(child(20,4)) :: c1
   class(child(:,4)), allocatable :: c2

   b1 = base(20,4)(2)
   allocate ( b2, source = base(20,4)(3) )
   c1 = child(20,4)(4,5.0)
   allocate ( c2, source = child(20,4)(6,7.0) )

   call b1%pp()

   do i =1, 10
      call b2%pp()
      call c1%pp()
   end do

   do i = 1, 100
      call c2%pp()
   end do

   print *, b1%i
   print *, b2%i
   print *, c1%i, c1%j
   print *, c2%i, c2%j

   do i =1, 10
      call b1%pp(2)
      call b2%pp(3)
      call c1%pp(2)
   end do

   do i = 1, 100
      call c2%pp(3)
   end do

   print *, b1%i
   print *, b2%i
   print *, c1%i, c1%j
   print *, c2%i, c2%j

   call c1%pp(10,5.0)
   call c2%pp(10,20.0)

   print *, c1%i, c1%j
   print *, c2%i, c2%j

   deallocate ( b2 )
   allocate ( b2, source = child(20,4) (100, 200) )

   call b2%pp()
   select type ( b2 )
      type is ( child(*,4) )
         print *, b2%i, b2%j
   end select

   call b2%pp(100)
   select type ( b2 )
      type is ( child(*,4) )
         print *, b2%i, b2%j
         call b2%pp(10,20.0)
         print *, b2%i, b2%j
   end select

   ! try generic interface

   b1 = base(20,4)(1)
   c1 = child(20,4)(1,2)

   deallocate ( b2, c2 )
   allocate ( b2, source = base(20,4)(3) )
   allocate ( c2, source = child(20,4)(3,4.0) )

   call pp(b1)
   call pp(b2)
   call pp(c1)
   call pp(c2)

   print *, b1%i
   print *, b2%i
   print *, c1%i, c1%j
   print *, c2%i, c2%j

   call pp(b1,10)
   call pp(b2,20)
   call pp(c1,30)
   call pp(c2,40)

   print *, b1%i
   print *, b2%i
   print *, c1%i, c1%j
   print *, c2%i, c2%j

   call pp(c1,10,5.0)
   call pp(c2,20,15.0)

   print *, c1%i, c1%j
   print *, c2%i, c2%j

   deallocate ( b2 )
   allocate ( b2, source = child(20,4) (100, 200) )

   call pp(b2)
   select type ( b2 )
      type is ( child(*,4) )
         print *, b2%i, b2%j
   end select

   call pp(b2,100)
   select type ( b2 )
      type is ( child(*,4) )
         print *, b2%i, b2%j
         call pp(b2,10,20.0)
         print *, b2%i, b2%j
   end select

end program
