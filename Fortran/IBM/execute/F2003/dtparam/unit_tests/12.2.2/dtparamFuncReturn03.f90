! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : James Ren
!*  DATE                       : 06/07/2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf2003
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

