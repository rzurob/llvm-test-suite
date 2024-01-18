! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : pos001ll
!*
!*  DATE                       : 2007-10-02 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Secition 9.9 INQUIRE Statement
!*                               - POS= specifier: Try using the pos specifier in various places
!*                                                 including main program DTIO.  Try it also with REWIND
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
   type, abstract :: base (lbase_1,lbase_2) ! lbase_1,lbase_2=1,3
      integer, len :: lbase_1,lbase_2
      character(lbase_1) :: c(lbase_2)
   contains
      procedure, pass :: getc
      procedure(intf), deferred :: getcc
   end type

   type, extends(base) :: child (lchild_1,lchild_2) ! lchild_1,lchild_2=1,3
      integer, len :: lchild_1,lchild_2
      character(lchild_1) :: cc(lchild_2)
   contains
      procedure, pass :: getcc
   end type

   interface
      function intf(a)
         import base
         class(base(*,*)), intent(in) :: a ! tcx: (*,*)
         character, dimension(3) :: intf
      end function
   end interface

contains

   function getc(a)
      class(base(*,*)), intent(in) :: a ! tcx: (*,*)
      character :: getc(3)
      getc = a%c
   end function

   function getcc(a)
      class(child(*,*,*,*)), intent(in) :: a ! tcx: (*,*,*,*)
      character :: getcc(3)
      getcc = a%cc
   end function

end module


program pos001ll
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*,*)), intent(inout) :: dtv ! tcx: (*,*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*,*)), intent(in) :: dtv ! tcx: (*,*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base(:,:)), allocatable :: b1, b2 ! tcx: (:,:)
   class(base(:,:)), pointer     :: b3, b4(:) ! tcx: (:,:)
   character(200) :: msg1 = ''
   integer :: stat1


   character(10) :: access1
   integer(4)    :: pos1

   ! allocation of variables

   allocate ( b1, source = child(1,3,1,3) ( (/'a','b','c'/), (/'A','B','C'/) ) ) ! tcx: (1,3,1,3)
   allocate ( b2, source = child(1,3,1,3) ( (/'d','e','f'/), (/'D','E','F'/) ) ) ! tcx: (1,3,1,3)
   allocate ( b3, source = child(1,3,1,3) ( (/'g','h','i'/), (/'G','H','I'/) ) ) ! tcx: (1,3,1,3)
   allocate ( b4(3), source = (/ b1, b2, b3 /) )

   ! I/O operations

   open ( 1, file = 'pos001ll.data', form='unformatted', access='stream' )

   inquire ( 1, iostat= stat1, iomsg=msg1, access=access1, pos = pos1 )     !<- check the initial pos and access mode for unit1

   if ( ( stat1 /= 0 ) .or. ( msg1 /= '' ) )                            error stop 101_4
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
use m1
   class(base(*,*)), intent(inout) :: dtv ! tcx: (*,*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(10) :: access1
   integer(4)    :: pos1, pos2

   inquire ( unit, access = access1, pos = pos1 )

   read ( unit, iostat = iostat, iomsg = iomsg )   dtv%c

   if ( iostat /= 0 )         error stop 26_4

   flush ( unit, iostat = iostat )

   if ( iostat /= 0 )         error stop 27_4

   select type ( dtv )
      type is (child(*,*,*,*)) ! tcx: (*,*,*,*)
         read ( unit, iostat = iostat ) dtv%cc
         if ( iostat /= 0 )   error stop 28_4
         flush ( unit )
         if ( iostat /= 0 )   error stop 29_4
   end select

   inquire ( unit, pos = pos2 )

   if ( pos2 /= pos1 + 6 )    error stop 30_4    !<- non-abstract child type contains 6 characters (therefore, always 6 pos written)

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base(*,*)), intent(in) :: dtv ! tcx: (*,*)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   character(10) :: access1
   integer(4)    :: pos1, pos2

   inquire ( unit, access = access1, pos = pos1 )

   write   ( unit, iostat = iostat, iomsg = iomsg )   dtv%getc()

   if ( iostat /= 0 )         error stop 31_4

   flush ( unit, iostat = iostat )

   if ( iostat /= 0 )         error stop 32_4

   select type ( dtv )
      type is (child(*,*,*,*)) ! tcx: (*,*,*,*)
         write ( unit, iostat = iostat ) dtv%getcc()
         if ( iostat /= 0 )   error stop 33_4
         flush ( unit )
         if ( iostat /= 0 )   error stop 34_4
   end select

   inquire ( unit, pos = pos2 )

   if ( pos2 /= pos1 + 6 )  error stop 35_4    !<- non-abstract child type contains 6 characters (therefore, always 6 pos written)

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1,lbase_1) to invoke with (1,3) / declare with (1,*) - 8 changes
! type: base - added parameters (lbase_1,lbase_2) to invoke with (1,3) / declare with (*,*) - 8 changes
! type: child - added parameters (lchild_1,lchild_2) to invoke with (1,3,1,3) / declare with (*,*,*,*) - 6 changes
