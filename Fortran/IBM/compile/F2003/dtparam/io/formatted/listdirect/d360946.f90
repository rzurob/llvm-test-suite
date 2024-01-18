!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : d360946.f
!*
!*  DATE                       : Jan. 13 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program d360946

  type :: dtp(l)
     integer,len  :: l
  end type

  implicit type(dtp(3))) (d) !<== syntax error

end program
