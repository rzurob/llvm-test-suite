! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/deferlen/functional/argAsso/deferLenArgAsso007.f
! opt variations: -qck -qnok -ql

!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : derived type containing scalar character with deferred length with
!*                               argument association, pointer component
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
   type base(k1)    ! (4)
       integer, kind :: k1
      character(:), pointer :: c
   end type

   type(base(4)) :: b1
   character(:), pointer :: c1

end module

program deferLenArgAsso007
   use n

   b1 = base(4)(null())
   allocate ( c1, source = 'worldcup' )

   b1%c => c1
   print *, len(b1%c)

   call foo ( b1 )

   print *,'outside: ', b1%c, len(b1%c)

   contains

      subroutine foo ( c )
         type(base(4)) :: c

         print *, 'inside: ', c%c, len(c%c)

         allocate ( c%c, source = c%c // " 2006" )

      end subroutine

end program
