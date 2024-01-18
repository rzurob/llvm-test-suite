! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : file001l
!*
!*  DATE                       : 2007-09-28 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Secition 9.9 INQUIRE Statement
!*                               - FILE= specifier: A file need not exist or connected to a unit
!*                                                  Any trailing blanks are ignored
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
   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
   end type
end module


program file001l
   use m1

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

   ! declaration of variables
   class(base(:)), allocatable :: b1, b2 ! tcx: (:)
   class(base(:)), pointer     :: b3, b4 ! tcx: (:)
   character(200) :: msg1
   integer :: stat1

   character(3)  :: c1

   integer :: size1
   logical :: exist1, named1, opened1
   character(13) :: name1

   ! allocation of variables

   allocate (base(3)::b1,b2,b3,b4) ! tcx: base(3)

   b1%c = 'ibm'
   b2%c = ''
   b3%c = 'ftn'
   b4%c = ''

   ! I/O operations

   open ( 2, file = 'file001l.data', form='unformatted', access='direct', recl=5 )

   INQUIRE ( file = 'file001l.tmp', size=size1, exist=exist1, name=name1, named=named1, opened=opened1 )      !<- file not connected to any unit
   print *, size1, exist1, name1, named1, opened1

   INQUIRE ( file = 'doesNotExist.tmp   ', size=size1, exist=exist1, named=named1, opened=opened1 )          !<- file does not exist
   print *, size1, exist1, named1, opened1

   INQUIRE ( file = 'file001l.data      ', size=size1, exist=exist1, named=named1, opened=opened1 )           !<- file does exist
   print *, size1, exist1, named1, opened1

   write (2, iostat=stat1, iomsg=msg1, rec = 4 )    b1
   if ( ( stat1 /= 0 ) .or. (msg1 /= 'DTIO write: file connected' ) ) error stop 6_4
   msg1 = ''

   read  (2, iostat=stat1, iomsg=msg1, rec = 4 )    b2
   if ( ( stat1 /= 0 ) .or. (msg1 /= 'DTIO read: file connected' ) )  error stop 7_4
   msg1 = ''

   if ( b2%c /= 'ibm' )                                               error stop 8_4

   INQUIRE ( file = 'file001l.data', size=size1, exist=exist1, name=name1, named=named1, opened=opened1 )                 !<- file does exist
   print *, size1, exist1, name1, named1, opened1

   ! close the file appropriately

   close ( 2, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(13) :: fileName = ''
    character(11) :: formName = ''
    logical :: opened1, exist1
    character(3) :: read1 = ''

    inquire ( unit, iostat=iostat, iomsg=iomsg, name=fileName,     &
                form=formName, opened=opened1, exist=exist1, read=read1 )
    if ( iostat /= 0 )                      error stop 101_4                 !<- ensure inquire was successful

    if ( (opened1) .and. (exist1) .and. (read1 == 'YES') ) then            !<- read from file only if file exists, is opened, and is readable
       if (( fileName   /= 'file001l.data') .and. (fileName /= 'fort.1') )  error stop 2_4
       if (  formName   /= 'UNFORMATTED' )   error stop 3_4
       read (unit, iostat=iostat, iomsg=iomsg )   dtv%c
       iomsg = 'DTIO read: file connected'
    else if ( (exist1) .and. (.not. opened1) )  then    !<- unit exists, but not connected to file
       iostat = 998
       iomsg = 'DTIO read: IMPOSSIBLE unit exists, not connected to file'
    else
       iostat = 999
       iomsg = 'DTIO read: IMPOSSIBLE unit and file do not exist or unreadable'
    end if


end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(3) :: write1 = ''
    character(11) :: formName = ''
    logical :: opened1, exist1

    inquire ( unit, iostat=iostat, iomsg=iomsg, write=write1,    &
               form=formName, opened=opened1, exist=exist1 )
    if ( iostat /= 0 )                      error stop 4_4        !<- ensure inquire was successful

    if ( (opened1) .and. (exist1) .and. (write1 == 'YES') ) then !<- read from file only if file exists, is opened, and is writeable
       if ( formName   /= 'UNFORMATTED' )   error stop 5_4
       write (unit, iostat=iostat, iomsg=iomsg ) dtv%c
       iomsg = 'DTIO write: file connected'
    else if ( (exist1) .and. (.not. opened1) )  then    !<- unit exists, but not connected to file
       iostat = 998
       iomsg = 'DTIO write: IMPOSSIBLE unit exists, not connected to file'
    else
       iostat = 999
       iomsg = 'DTIO write: IMPOSSIBLE unit and file do not exist or unreadable'
    end if

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 6 changes
