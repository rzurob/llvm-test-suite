! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Secition 9.9 INQUIRE Statement
!*                               - POS= specifier: Try using the pos= specifier in various places
!*                                                 including main program DTIO. and have multiple write statements inside DTIO
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type, abstract :: base
      character :: c(3)
   contains
      procedure, pass :: getc
      procedure(intf), deferred :: getcc
   end type

   type, extends(base) :: child
      character :: cc(3)
   contains
      procedure, pass :: getcc
   end type

   interface
      function intf(a)
         import base
         class(base), intent(in) :: a
         character, dimension(3) :: intf
      end function
   end interface

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

contains

   function getc(a)
      class(base), intent(in) :: a
      character :: getc(3)
      getc = a%c
   end function

   function getcc(a)
      class(child), intent(in) :: a
      character :: getcc(3)
      getcc = a%cc
   end function

end module


program pos004
   use m1

   ! declaration of variables
   class(base), allocatable :: b1, b2
   class(base), pointer     :: b3, b4(:)
   character(200) :: msg1 = ''
   integer :: stat1


   character(10) :: access1
   integer(4)    :: pos1

   ! allocation of variables

   allocate ( b1, source = child ( (/'a','b','c'/), (/'A','B','C'/) ) )
   allocate ( b2, source = child ( (/'d','e','f'/), (/'D','E','F'/) ) )
   allocate ( b3, source = child ( (/'g','h','i'/), (/'G','H','I'/) ) )
   allocate ( b4(3), source = (/ b1, b2, b3 /) )
   ! I/O operations

   open ( 1, file = 'pos004.data', form='unformatted', access='stream' )

   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1 )     !<- check the initial pos and access mode for unit1

   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 1_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 1 ) )                    error stop 2_4

   write ( 1, iostat = stat1, iomsg=msg1, pos = 7 )                     b1  !<- writes 'abcABC' to pos 7-12
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 3_4
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 4_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 13 ) )                   error stop 5_4

   write ( 1, iostat = stat1, iomsg=msg1, pos = 1 )                     b2  !<- writes 'defDEF' to pos 1-6
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 6_4
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 7_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 7 ) )                    error stop 8_4  !<- pos of the file should be immediately after last write

   write ( 1, iostat = stat1, iomsg=msg1, pos = 13 )                    b3  !<- writes 'ghiGHI' to pos 13-18
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 9_4
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 10_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 19 ) )                   error stop 11_4  !<- pos of the file should be immediately after last write

   write ( 1, iostat = stat1, iomsg=msg1, pos = pos1 )                  b4  !<- writes 'abcABCdefDEFghiGHI' to pos 19-37
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 12_4
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 13_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 37 ) )                   error stop 14_4  !<- pos of the file should be immediately after last write

   rewind 1

   ! memory map after writing
   ! pos  contnt   pos
   ! 1    defDEF   6
   ! 7    abcABC   12
   ! 13   ghiGHI   18
   ! 19   abcABC   24
   ! 25   defDEF   30
   ! 31   ghiGHI   36

   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 15_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 1 ) )                    error stop 16_4  !<- pos of the file should be reset to 1

   read ( 1, iostat = stat1, iomsg=msg1, pos = 4 )                      b4
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 17_4
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 18_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 22 ) )                   error stop 19_4  !<- pos of the file should be immediately after last read

   read ( 1, iostat = stat1, iomsg=msg1 )                               b1               !<- last pos was 22
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 20_4
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 21_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 28 ) )                   error stop 22_4  !<- pos of the file should be immediately after last read

   read ( 1, iostat = stat1, iomsg=msg1, pos = 31 )                     b3
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 23_4
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 24_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 37 ) )                   error stop 25_4  !<- pos of the file should be immediately after last read


   ! check if the values are set correctly

   print *, b1%getc()             ! ABC
   print *, b1%getcc()            ! def
   print *, b3%getc()             ! ghi
   print *, b3%getcc()            ! GHI
   print *, b4(1)%getc()          ! DEF
   print *, b4(1)%getcc()         ! abc
   print *, b4(2)%getc()          ! ABC
   print *, b4(2)%getcc()         ! ghi
   print *, b4(3)%getc()          ! GHI
   print *, b4(3)%getcc()         ! abc

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer(4)    :: pos1, pos2, pos3, pos4, pos5

   inquire ( unit, pos = pos1 )

   read ( unit, iostat = iostat, iomsg = iomsg )   dtv%c(1)
   if ( iostat /= 0 ) error stop 26_4
   inquire ( unit, pos = pos2 )

   read ( unit, iostat = iostat, iomsg = iomsg )   dtv%c(2)
   if ( iostat /= 0 ) error stop 27_4
   inquire ( unit, pos = pos3 )

   read ( unit, iostat = iostat, iomsg = iomsg )   dtv%c(3)
   if ( iostat /= 0 ) error stop 28_4
   inquire ( unit, pos = pos4 )

   select type ( dtv )
      type is (child)
         read ( unit, iostat = iostat ) dtv%cc
         if ( iostat /= 0 ) error stop 29_4
   end select

   inquire ( unit, pos = pos5 )

   ! check each value of pos

   if ( pos2 /= pos1 + 1 )    error stop 30_4
   if ( pos3 /= pos1 + 2 )    error stop 31_4
   if ( pos4 /= pos1 + 3 )    error stop 32_4
   if ( pos5 /= pos1 + 6 )    error stop 33_4     !<- non-abstract child type contains 6 characters (therefore, always 6 pos written)

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   integer(4)    :: pos1, pos2, pos3, pos4, pos5

   inquire ( unit, pos = pos1 )

   write ( unit, iostat = iostat, iomsg = iomsg )   dtv%c(1)
   if ( iostat /= 0 ) error stop 34_4
   inquire ( unit, pos = pos2 )

   write ( unit, iostat = iostat, iomsg = iomsg )   dtv%c(2)
   if ( iostat /= 0 ) error stop 35_4
   inquire ( unit, pos = pos3 )

   write ( unit, iostat = iostat, iomsg = iomsg )   dtv%c(3)
   if ( iostat /= 0 ) error stop 36_4
   inquire ( unit, pos = pos4 )

   select type ( dtv )
      type is (child)
         write ( unit, iostat = iostat ) dtv%cc
         if ( iostat /= 0 ) error stop 37_4
   end select

   inquire ( unit, pos = pos5 )

   ! check each value of pos

   if ( pos2 /= pos1 + 1 )    error stop 38_4
   if ( pos3 /= pos1 + 2 )    error stop 39_4
   if ( pos4 /= pos1 + 3 )    error stop 40_4
   if ( pos5 /= pos1 + 6 )    error stop 41_4     !<- non-abstract child type contains 6 characters (therefore, always 6 pos written)

end subroutine
