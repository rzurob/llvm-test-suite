!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d359876.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Dec. 5 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :  
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
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

