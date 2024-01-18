!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 04/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : GENERIC BINDING:
!*                                  Cross Feature: Structure Constructor
!*                                    -  Structure constructor with pointer/allocatable components unformatted i/o
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type base
      character(3) :: c
      contains
         procedure, pass :: write => writeb
         generic :: write(unformatted) => write
   end type

   type, extends(base) :: child
      character(3), pointer :: i => null()
      contains
         procedure, pass :: write => writec
   end type

   type, extends(child) :: gen3
      character(3), pointer :: s => null()
      contains
         procedure, pass :: write => writeg
   end type

   contains

      subroutine writeb (dtv, unit, iostat, iomsg)
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write (unit, iostat=iostat, iomsg=iomsg) dtv%c
         iomsg = 'dtiowriteb'

      end subroutine

      subroutine writec (dtv, unit, iostat, iomsg)
         class(child), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         if ( .not. associated( dtv%i ) ) then
            write (unit, iostat=iostat, iomsg=iomsg) dtv%c, 'xxx'
         else
            write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         end if

         iomsg = 'dtiowritec'

      end subroutine

      subroutine writeg (dtv, unit, iostat, iomsg)
         class(gen3), intent(in) :: dtv
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg


         if ( .not. associated( dtv%i ) ) then
            write (unit, iostat=iostat, iomsg=iomsg) dtv%c, 'yyy'
         else
            write (unit, iostat=iostat, iomsg=iomsg) dtv%c, dtv%i
         end if

         if ( associated( dtv%s ) ) then
            write (unit, iostat=iostat, iomsg=iomsg) dtv%s
         else
            write (unit, iostat=iostat, iomsg=iomsg) 'zzz'
         end if

         iomsg = 'dtiowriteg'

      end subroutine

end module

program structConstr003a
   use m

   integer :: stat
   character(200) :: msg

   character(3), target :: aaa = 'AAA'
   character(3), target :: bbb = 'BBB'
   character(3), target :: zoo = 'ZOO'

   character(3)  :: c1
   character(6)  :: c2
   character(9)  :: c3
   character(6)  :: c4
   character(9)  :: c5
   character(45) :: c6

   open ( 1, file = 'structConstr003a.1', form='unformatted', access='sequential' )

   write ( 1, iostat = stat, iomsg = msg )                   base('abc')
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteb' ) )         error stop 1_4

   write ( 1, iostat = stat, iomsg = msg )      child('def')
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )         error stop 2_4

   write ( 1, iostat = stat, iomsg = msg )    gen3('ghi')
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )         error stop 3_4

   write ( 1, iostat = stat, iomsg = msg )      child('jkl', aaa )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowritec' ) )         error stop 4_4

   write ( 1, iostat = stat, iomsg = msg )      gen3('mno', bbb, zoo )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )         error stop 5_4

   write ( 1, iostat = stat, iomsg = msg )    ( child (zoo,aaa), gen3('cat',bbb, zoo),i=-1,1 )
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowriteg' ) )         error stop 6_4

   rewind 1

   read ( 1, iostat = stat, iomsg = msg )       c1
   read ( 1, iostat = stat, iomsg = msg )       c2
   read ( 1, iostat = stat, iomsg = msg )       c3
   read ( 1, iostat = stat, iomsg = msg )       c4
   read ( 1, iostat = stat, iomsg = msg )       c5
   read ( 1, iostat = stat, iomsg = msg )       c6
   
   if ( ( c1 /= 'abc' ) .or. ( c2 /= 'defxxx' )  .or. ( c3 /= 'ghiyyyzzz' ) .or. (c4 /= 'jklAAA' ) .or. ( c5 /= 'mnoBBBZOO' ) .or. &
        ( c6 /= 'ZOOAAAcatBBBZOOZOOAAAcatBBBZOOZOOAAAcatBBBZOO' ) ) error stop 7_4
       
   close ( 1, status = 'delete' )


end program
