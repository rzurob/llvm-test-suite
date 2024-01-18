! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : parameter101kl
!*
!*  PROGRAMMER                 : David Forster (derived from parameter101 by Robert Ma)
!*  DATE                       : 2007-07-06 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try namelist formatting with dummy argument
!*                                        and associating with named-constant actual arg (input)
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
   type base (kb)
      integer, kind :: kb
      integer(kb) :: i
   end type

   type, extends(base) :: child (kc)
      integer, kind :: kc
      real(kc) :: r
   end type

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base(4)), intent(inout) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   procedure(logical) :: precision_r4

   contains

   subroutine readNamedConst ( unit, b1 )
      integer, intent(in) :: unit
      class(base(4)), intent(inout) :: b1 ! tcx: (4)

      integer :: stat
      character(150) :: msg

      namelist /nml/ b1
      read (unit, nml, iostat = stat, iomsg = msg)
      if ( ( stat /= 0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

      select type ( b1 )
         type is (base(4)) ! tcx: (4)
            if ( b1%i /= 111 ) error stop 2_4
         type is (child(4,4)) ! tcx: (4,4)
            if ( ( b1%i /= 111 ) .or. ( .not. precision_r4(b1%r,11.11) ))  error stop 3_4
         class default
            error stop 4_4
      end select

   end subroutine

end module

program parameter101kl
use m

   integer :: stat
   character(200) :: msg = ''
   type(base(4)), parameter  :: b1 = base(4) (777) ! tcx: (4) ! tcx: (4)
   type(child(4,4)), parameter :: b2 = child(4,4)(888,8.88) ! tcx: (4,4) ! tcx: (4,4)

   open (1, file = 'parameter101kl.1', form='formatted', access='sequential' )

   call readNamedConst(1, b1)
   call readNamedConst(1, b2)

end program

subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
use m, only: base, child

   class(base(4)), intent(inout) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 6_4
   if ( size(v_list, 1) /= 0 ) error stop 7_4

   read (unit, "(1X,I4)", iostat=iostat )          dtv%i

   select type (dtv)
      type is (child(4,4)) ! tcx: (4,4)
         read (unit, "(1X,f8.3)", iostat=iostat )  dtv%r
   end select

   iomsg = 'dtioread'

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 6 changes
! type: child - added parameters (kc) to invoke with (4,4) / declare with (4,4) - 4 changes
