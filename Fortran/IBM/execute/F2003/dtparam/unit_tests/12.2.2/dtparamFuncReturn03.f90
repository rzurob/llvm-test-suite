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

type base(baseKind, baseLen)
   integer, kind :: baseKind
   integer, len :: baseLen
   integer(baseKind) :: baseId(baseLen)
end type

type(base(4, 10)) :: base1
type(base(4, 5)) :: base2
base1%baseId = (/((i * i), i = 1, 10)/)

base2 = funn(base1)

print *, base2%baseid

contains
function funn(arg)
   type(base(4, *)) :: arg
   type(base(4, 5)) :: funn
   funn = arg
end function
end

