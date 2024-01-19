! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qdeferredlp /tstdev/F2003/deferlen/functional/argAsso/deferLenArgAsso011.f
! opt variations: -qnock -qnok -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : character with deferred length
!*                               in user-defined assignment
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module n

   type base(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
      character(:), allocatable :: c
   end type

   interface assignment(=)
      subroutine mycharassignment(a,b)
         import base
         type(base(4,*)), intent(out)  :: a
         character(*), intent(in) :: b
      end subroutine
   end interface

end module

subroutine mycharassignment(a,b)
   use n, only: base
   type(base(4,*)), intent(out)  :: a
   character(*), intent(in) :: b

   if( allocated( a%c) ) deallocate ( a%c )

   allocate ( a%c, source = b )

end subroutine

program deferLenArgAsso011
   use n

   type(base(4,:)), pointer :: b1

   character(:), allocatable :: c1

   allocate ( b1, source = base(4,20)('abcdefghi') )

   b1 = 'IBM'

   allocate ( c1, source = 'XLFortran' )

   print *, b1%c, len(b1%c)

   b1 = c1

   print *, b1%c, len(b1%c)

end program
