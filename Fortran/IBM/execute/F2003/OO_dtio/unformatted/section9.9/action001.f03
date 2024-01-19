! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Secition 9.9 INQUIRE Statement
!*                               - ACCESS= specifier: Test if the ACTION specifier is set correctly
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program action001
   use ISO_FORTRAN_ENV

   ! declaration of variables
   character(200) :: msg1
   integer :: stat1

   character(10) :: action1
   character(3)  :: stream1

   ! open files

   INQUIRE ( 1, iostat=stat1, action=action1 )

   if ( action1 /= 'UNDEFINED' )   error stop 1_4

   open ( 1, file='action001.1', form='unformatted', action='read' )
   open ( 2, file='action001.2', form='unformatted', action='write', access='direct', recl=5 )
   open ( 3, file='action001.3', form='unformatted', action='readwrite',access='stream' )
   open ( 4, file='action001.4', form='unformatted', access='stream' )

   ! INQUIRE operations

   INQUIRE ( 1, iostat=stat1, action=action1 )
   if ( action1 /= 'READ' )           error stop 1_4

   INQUIRE ( file='action001.2', iostat=stat1, action=action1 )
   if ( action1 /= 'WRITE' )          error stop 2_4

   INQUIRE ( 3, iostat=stat1, action=action1 )
   if ( action1 /= 'READWRITE' )      error stop 3_4

   INQUIRE ( file='action001.4', iostat=stat1, action=action1 )
   if ( action1 /= 'READWRITE' )      error stop 4_4

   close (4)

   open ( 7, file = 'action001.4', status='old', action='read' )

   INQUIRE ( file='action001.4', iostat=stat1, action=action1 )
   if ( action1 /= 'READ' )           error stop 5_4

   ! inquire the standardIO

   INQUIRE (ERROR_UNIT, action=action1 )
   if ( action1 /= 'READWRITE' )      error stop 6_4
   INQUIRE (INPUT_UNIT, action=action1 )
   if ( action1 /= 'READWRITE' )      error stop 7_4
   INQUIRE (OUTPUT_UNIT, action=action1 )
   if ( action1 /= 'READWRITE' )      error stop 8_4


   ! close the file appropriately

   close ( 1, status ='delete' )
   close ( 2, status ='delete' )
   close ( 3, status ='delete' )
   close ( 7, status ='delete' )

end program
