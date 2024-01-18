!*******************************************************************************
!*  ============================================================================
!*
!*  TEST CASE NAME             : BASync
!*
!*  DATE                       : 2010-12-30
!*
!*  PRIMARY FUNCTIONS TESTED   : F2008 BLOCK
!*  SECONDARY FUNCTIONS TESTED : var declared in containing scope marked as async in block
!*
!*  DESCRIPTION
!*
!*  Read data in synchronously and then asynchronously, and verify that the
!*  appropriate variables are marked appropriately in the listing file.
!*  Unfortunately, only variables which are asynchronous throughout the program
!*  are marked in the listing.  ASYNCHRONOUS does not show up on individual WCODE
!*  instructions, since it is really only relevant during I/O, and then only
!*  addresses are passed around (and LDA instructions are not marked specially,
!*  unlike VOLATILE).  Consequently, our verification of the application of the
!*  ASYNCHRONOUS attribute will be a little undernourished.
!*
!* ============================================================================
!234567890123456789012345678901234567890123456789012345678901234567890123456789

program BASync

  ! Use the default implicit declaration - I-N is I4, everything else is R4:
  integer, parameter :: DATA_WIDTH = 4
  integer, parameter :: ASIZE = 1000
  integer, parameter :: IN_UNIT = 10
  integer(DATA_WIDTH) :: d1obvious(ASIZE), i0obvious
  integer :: idvar

  open(IN_UNIT, file="BASync.in", status="OLD", access="DIRECT", asynch="YES", recl=(ASIZE+1)*DATA_WIDTH)
  read(IN_UNIT, rec=1) i0obvious, d1obvious(1:ASIZE/2)  ! Read in 1st set of data
                                                        ! d1obvious implicitly given the asynchronous attribute by the read

  print *, trim(identifyType('o.d1o',d1obvious(1)))
  print *, trim(identifyType('o.i0o',i0obvious))

  block
    asynchronous :: i1obvious
    ! Start asynch read on 2nd set of data
    read(IN_UNIT, id=idvar, rec=2) i1obvious, d1obvious(ASIZE/2+1:ASIZE)
    ! process while reading 2nd set
    if (any(d1obvious(1:ASIZE/2) /= [(i0obvious+i,i=1,ASIZE/2)])) stop 10
    wait(id=idvar)
    if (any(d1obvious(ASIZE/2+1:ASIZE) /= [(i1obvious+i,i=1,ASIZE/2)])) stop 11
    print *, trim(identifyType('i.d1o',d1obvious(ASIZE)))
    print *, trim(identifyType('i.i1o',i1obvious))
  end block

  read(IN_UNIT, rec=3) i0obvious, d1obvious(1:ASIZE/2)  ! Read in last set of data

  print *, trim(identifyType('x.d1o',d1obvious(1)))
  print *, trim(identifyType('x.i0o',i0obvious))
  if (any(d1obvious(1:ASIZE/2) /= [(i0obvious+i,i=1,ASIZE/2)])) stop 12

  close(unit=IN_UNIT)

contains

  elemental character(200) function identifyType(label,arg)
    character(4), intent(in) :: label
    class(*), intent(in) :: arg
    select type(arg)
      type is (integer(4));   write(identifyType,*) label, '.i4', kind(arg), arg
      type is (real(4));      write(identifyType,*) label, '.r4', kind(arg), arg
      class default;          write(identifyType,*) label, ' invalid'
    end select
  end function

end program BASync
