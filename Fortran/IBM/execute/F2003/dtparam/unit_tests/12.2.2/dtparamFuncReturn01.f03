! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/07/2007
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : function return with DTP
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

type base(baseKind)
   integer, kind :: baseKind
   integer(baseKind) :: baseId
end type

type(base(4)) :: base1, base2
base1%baseId = 10_4

base2 = funn(base1)

if (base2%baseid .ne. 10_4) error stop 1

contains
function funn(arg)
   type(base(4)) :: funn, arg
   funn = arg
end function
end
