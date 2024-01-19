! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2009-10-15
!*
!*  DESCRIPTION                : miscellaneous (defect 370664)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program funcRet005
  type base(n1)
     integer, len      :: n1
     real, allocatable :: data
  end type base

  type container
     type(base(20)) :: data
  end type container

  type(container), allocatable :: co1(:)

  co1 = [(container(base(20)(0.0)), j=1,1)]

end

