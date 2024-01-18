!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 5 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  defect 359876
!234567890123456789012345678901234567890123456789012345678901234567890

program d359876
  implicit none

  integer :: i1=11
  open(10,file="d359876.out",action='write',sign='plus')

  write(10,'(i7.4)') i1
  write(10,'(i7)') i1
  write(10,'(sp,i7.4)') i1
  write(10,'(sp,i7)') i1
  write(10,'(ss,i7.4)') i1
  write(10,'(ss,i7)') i1

  close(10)

end program

