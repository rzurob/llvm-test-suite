!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: access001.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Secition 9.9 INQUIRE Statement
!*                               - ACCESS= specifier: Test if the ACCESS specifier is set correctly 
!*                                                    (SEQUENTIAL, DIRECT, STREAM, UNDEFINED )
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program access001

   ! declaration of variables
   character(200) :: msg1
   integer :: stat1
   
   character(10) :: access1
   character(3)  :: stream1
    
   ! open files
   
   open ( 1, file='access001.1', form='unformatted' )                           !<- default access is "SEQUENTIAL"
   open ( 2, file='access001.2', form='unformatted', access='direct', recl=5 )  !<- access is "direct"  <- lower case (inquire should return UPPER CASE)
   open ( 3, file='access001.3', form='unformatted', access='stream' )          !<- access is "stream"  <- lower case (inquire should return UPPER CASE)
   
   ! INQUIRE operations
   
   INQUIRE ( 1, iostat=stat1,  access=access1 )                             !<- inquire by unit
   
   if ( (access1 /= 'SEQUENTIAL') .or. ( stat1 /= 0 ) )                     error stop 1_4

   INQUIRE ( file='access001.2', access=access1 )                           !<- inquire by file
   
   if ( (access1 /= 'DIRECT') .or. ( stat1 /= 0 ))                          error stop 2_4

   INQUIRE ( file='access001.3', access=access1 )                           !<- inquire by file
   
   if ( (access1 /= 'STREAM') .or. ( stat1 /= 0 ) )                         error stop 3_4
   
   INQUIRE ( unit=0, iostat=stat1, access=access1 )                         !<- inquire by unit on stderr (sequential access)

   if ( (access1 /= 'SEQUENTIAL') .or. ( stat1 /= 0 ) )                     error stop 4_4
   
   INQUIRE ( file='doesNotExist.data', iostat=stat1, access=access1 )       !<- inquire by file on a does not exist file

   if ( (access1 /= 'UNDEFINED') .or. ( stat1 /= 0 ) )                      error stop 5_4
   
   INQUIRE ( 99 , iostat=stat1, access=access1 )                            !<- inquire by unit on unconnected unit

   if ( (access1 /= 'UNDEFINED') .or. ( stat1 /= 0 ) )                      error stop 6_4
   
   close (3)
    
   INQUIRE ( file='access001.3', iostat=stat1,  access=access1 )            !<- inquire by file on a unconnected file
   
   if ( (access1 /= 'UNDEFINED') .or. ( stat1 /= 0 ) )                      error stop 7_4
   
   open (3, file='access001.3', status = 'old' )
   
   INQUIRE ( file='access001.3', iostat=stat1,  access=access1, stream=stream1 )            !<- inquire by file that is unconnected

   if ( (access1 /= 'SEQUENTIAL') .or. ( stat1 /= 0 ) .or. ( stream1 /= "NO") )   error stop 8_4
   
   
   ! close the file appropriately

   close ( 1, status ='delete' )
   close ( 2, status ='delete' )
   close ( 3, status ='delete' )
   
end program
