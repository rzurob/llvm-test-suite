! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-02 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Secition 9.9 INQUIRE Statement
!*                               - POS= specifier: Try using the pos specifier inside DTIO and main program
!*                                                 Try it with zero sized derived types
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
   type, abstract :: base (lbase) ! lbase=0
      integer, len :: lbase
   end type

   type, extends(base) :: child
   end type

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module


program pos003l
   use m1

   ! declaration of variables
   class(base(:)), allocatable :: b1, b2 ! tcx: (:)
   character(200) :: msg1 = ''
   integer :: stat1

   character(10) :: access1
   integer(4)    :: pos1
   integer(4)    :: size1

   ! allocation of variables

   allocate ( b1, source = child(0)() ) ! tcx: (0)
   allocate ( b2, source = child(0)() ) ! tcx: (0)

   ! I/O operations


   open ( 1, file = 'pos003l.data', form='unformatted', access='stream' )

   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1, size=size1 )        !<- check the initial pos and access mode for unit1
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                                               error stop 101_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 1 ) )                                       error stop 2_4  !<- pos of the file should be reset to 1

   write ( 1, iostat=stat1, iomsg=msg1, pos=4 )                 b1                         !<- write to file starting at pos 1
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1, size=size1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                                               error stop 3_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 4 ) .or. ( size1 /= 0 ) )                    error stop 4_4

   write ( 1, iostat=stat1, iomsg=msg1, pos=1 )          b2
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1, size=size1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                                               error stop 5_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 1 ) .or. ( size1 /= 0 ) )                    error stop 6_4

   endfile 1     !<- end file at pos 1, therefore size of file is still 0

   write ( 1, iostat=stat1, iomsg=msg1, pos=4 )          b1,b2
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1, size=size1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                                               error stop 7_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 4 ) .or. ( size1 /= 0 ) )                    error stop 8_4

   endfile 1    !<- end file at pos 4, therefore size of file is 3

   rewind 1
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1, size=size1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                                                error stop 9_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 1 ) .or. ( size1 /= 3 ) )                    error stop 10_4  !<- pos of the file should be reset to 1

   read (1, iostat = stat1, iomsg = msg1, pos=3 ) b1, b2                !<- read a position right before the end of file
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1, size=size1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                                                error stop 11_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 3 ) .or. ( size1 /= 3 ) )                    error stop 12_4  !<- pos of the file should be reset to 1

   rewind 1
   endfile 1

   read (1, iostat = stat1, iomsg = msg1, pos=1 ) b1, b2                                    !<- read a position right before the end of file
   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1, size=size1 )
   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                                                error stop 13_4
   if ( ( access1 /= 'STREAM' ) .or. ( pos1 /= 1 ) .or. ( size1 /= 0 ) )                    error stop 14_4  !<- pos of the file should be reset to 1

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base(*)), intent(inout) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(0)  :: tmp = ''
   character(10) :: access1
   integer(4)    :: pos1, pos2

   inquire ( unit, access = access1, pos = pos1 )

   flush ( unit, iostat = iostat )

   if ( iostat /= 0 )            error stop 15_4

   select type ( dtv )
      class is (child(*)) ! tcx: (*)
      flush (unit, iostat = iostat )
      if ( iostat /= 0 )         error stop 16_4
   end select

   read ( unit, iostat = iostat, iomsg = iomsg ) tmp
   if ( iostat /= 0 ) error stop 17_4

   inquire ( unit, pos = pos2 )

   if ( pos2 /= pos1 )           error stop 18_4   !<- pos shall not be changed at the end of the dtio procedure

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1, only: base, child
   class(base(*)), intent(in) :: dtv ! tcx: (*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(10) :: access1
   integer(4)    :: pos1, pos2
   character(0)  :: tmp = ''

   inquire ( unit, access = access1, pos = pos1 )

   if ( iostat /= 0 )         error stop 19_4

   flush ( unit, iostat = iostat )

   if ( iostat /= 0 )         error stop 20_4

   select type ( dtv )
      class is (child(*)) ! tcx: (*)
         flush ( unit )
         if ( iostat /= 0 )   error stop 21_4
   end select

   write ( unit, iostat = iostat, iomsg = iomsg ) tmp
   if ( iostat /= 0 ) error stop 22_4


   inquire ( unit, pos = pos2 )

   if ( pos2 /= pos1     )    error stop 23_4   !<- pos shall not be changed at the end of the dtio procedure

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase) to invoke with (0) / declare with (*) - 5 changes
! type: child - added parameters () to invoke with (0) / declare with (*) - 4 changes
