! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                :
!*                               DTP kind for TO and FROM are different
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type t(k)
    integer, kind :: k
    integer(k) :: id
end type

    class(t(4)), allocatable :: TO
    type(t(8)), allocatable :: FROM

    call move_alloc(FROM, TO)

    end
