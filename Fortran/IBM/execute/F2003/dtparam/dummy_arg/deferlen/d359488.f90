!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 27 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  DEFECT 359488
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1)
       integer,len :: l1
       character(l1) :: firstname
   end type
   type,extends(base) :: child(l2)
       integer,len    :: l2
       character(20)  :: lastname
   end type
end module

program d359488
  use m
  implicit none

  type(child(15,20)),target  :: tchild(2)
  class(base(15)),pointer :: base(:)

  tchild=[child(15,20)(firstname="JENNIFER",lastname="GARDEN"),&
          child(15,20)(firstname="TOM",lastname="CRUISE")]
  allocate(base(2),source=tchild)
  select type(base)
    type is(child(*,*))
       print *,base(1)%firstname,base(1)%lastname
       print *,(base(1)%firstname /= "JENNIFER"),(base(1)%lastname /= "GARDEN")
    class default
       stop
  end select

end program

