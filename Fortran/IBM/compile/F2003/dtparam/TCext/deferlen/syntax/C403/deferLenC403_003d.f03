! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/deferlen/syntax/C403/deferLenC403_003d.f
! opt variations: -qck -qnok -qnol

!*  ===================================================================
!*
!*  DATE                       : 05/01/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DESCRIPTION                : C403: character with deferred length as structure component without
!*                                     pointer or allocatable attributes
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

   type base(k1,n1)    ! (4,20)
       integer, kind :: k1
       integer, len  :: n1
   end type

   type, extends(base) :: child    ! (4,20)
      character(:) :: c1
   end type

end module

program deferLenC403_001d

   character(:) c2

   contains

      subroutine bar(c3)
         character(:) :: c3
      end subroutine

end program

character(:) function c4()
end function