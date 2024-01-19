!***************************************************************************

!*  ===================================================================
!*
!*  DATE                       : Feburary 18, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : ENDFILE
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS : 3
!*
!*  DESCRIPTION                : The UNIT command has to be in the range 1
!*                               through 2147483647, also I/O statement ENDFILE
!*                               is only for file connected for sequential
!*                               access. The iomsg specifier was passed in by
!*                               element of a string array in a derived type
!*                               variable which is a member of another derived
!*                               type.
!*
!*  TEST CONDITIONS            : 1) I/O statements with unit number -9.
!*                               2) ENDFILE stmt with direct unformatted file.
!*                               3) ENDFILE stmt with direct formatted file.
!* ===================================================================
!*  REVISION HISTORY
!*  MM/DD/YY:  Init:  Comments:
!*  02/18/04   RL     Initial version
!*
!*********************************************************************

      program fxiomsgl019

      implicit none                     ! All variables must be Declared

        TYPE message

           integer(4)      useless
           character(300)  errmsg(5)

        END TYPE message

        TYPE message2

           integer          useless2
           TYPE ( message ) errmsg

        END TYPE message2

      integer*4 case_id                 ! Test Case id under test.
      integer*4 a, ios
      TYPE( message2 ) t_msg(3)

!
!  Unit number too small ( unit = -9 )
!

      a = -9

!
! TestCase 1...
!

      case_id = case_id + 1

      endfile ( unit = a, err = 10, iostat = ios, iomsg = &
  &   t_msg(2)%errmsg%errmsg(4) )

      call zzrc ( case_id )

 10   write(18,*) t_msg(2)%errmsg%errmsg(4)

      if ( ios <> 36 ) call zzrc ( case_id )

      open ( 9, access = 'DIRECT', recl = 80 )

      open ( 8, form = 'FORMATTED', access = 'DIRECT', recl = 80 )

!
! TestCase 2...
!

      case_id = case_id + 1

      endfile ( 9, err = 20, iostat =ios, iomsg = t_msg(2)%errmsg%errmsg(4) )

      call zzrc ( case_id )

 20   write(18,*) t_msg(2)%errmsg%errmsg(4)

      if ( ios <> 17 ) call zzrc ( case_id )

!
! TestCase 3...
!

      case_id = case_id + 1

      endfile ( 9, err = 30, iostat =ios, iomsg = t_msg(2)%errmsg%errmsg(4) )

      call zzrc ( case_id )

 30   write(18,*) t_msg(2)%errmsg%errmsg(4)

      if ( ios <> 17 ) call zzrc ( case_id )

! Clean up...

      close ( 8, status = 'DELETE' )

      close ( 9, status = 'DELETE' )

      end                     ! End of TestCase.
