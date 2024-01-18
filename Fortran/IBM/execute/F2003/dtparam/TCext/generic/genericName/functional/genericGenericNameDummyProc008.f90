! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/genericName/functional/genericGenericNameDummyProc008.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: generic tb dummy arg has a dummy procedure
!*                                             and using dummy proc which one is subroutine the other is function
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
         procedure, pass :: printbase
         procedure, pass :: printbasewithchk

         generic :: print => printbase, printbasewithchk

   end type

   abstract interface
      subroutine sub(b)
         import base
         class(base(*,4)), intent(in) :: b
      end subroutine
   end interface

   abstract interface
      integer function func(b)
         import base
         class(base(*,4)), intent(in) :: b
      end function
   end interface

   interface
      subroutine iformat (b)
         import base
         class(base(*,4)), intent(in) :: b
      end subroutine
   end interface

   interface
      integer function iformatwchk (b)
         import base
         class(base(*,4)), intent(in) :: b
      end function
   end interface

   interface
      subroutine listdirected (b)
         import base
         class(base(*,4)), intent(in) :: b
      end subroutine
   end interface

   interface
      integer function listdirectedwchk (b)
         import base
         class(base(*,4)), intent(in) :: b
      end function
   end interface

   contains

      subroutine printbase( a, b )
         class(base(*,4)), intent(inout) :: a
         procedure(sub) :: b

         call b(a)

      end subroutine

      subroutine printbasewithchk( a, b )
         class(base(*,4)), intent(inout) :: a
         procedure(func) :: b

         integer :: iostat

         iostat = b(a)
         if ( iostat /= 0 ) error stop 1_4

      end subroutine

end module

program genericGenericNameDummyProc008
   use m

   type(base(:,4)), pointer :: b1
   class(base(:,4)), allocatable :: b2


   allocate ( base(20,4):: b1, b2 )

   b1%i = 100_4
   b2%i = 200_4

   call b1%print(iformat)
   call b1%print(iformatwchk)

   call b2%print(iformat)
   call b2%print(iformatwchk)

   call b1%print(listdirected)
   call b1%print(listdirectedwchk)

   call b2%print(listdirected)
   call b2%print(listdirectedwchk)

end program

subroutine iformat (b)
   use m, only: base
   class(base(*,4)), intent(in) :: b

   write (*,"('iformat:',I14)") b%i

end subroutine

integer function iformatwchk (b)
   use m, only: base
   class(base(*,4)), intent(in) :: b

   write (*,"('iformatwchk:',I10)", iostat = iformatwchk) b%i

end function

subroutine listdirected (b)
   use m, only: base
   class(base(*,4)), intent(in) :: b

   write (*,*) 'listdirected:    ', b%i

end subroutine

integer function listdirectedwchk (b)
   use m, only: base
   class(base(*,4)), intent(in) :: b

   write (*,*, iostat = listdirectedwchk) 'listdirectedwchk:', b%i

end function


