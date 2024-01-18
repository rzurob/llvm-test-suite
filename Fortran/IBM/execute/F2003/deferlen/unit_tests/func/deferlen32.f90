!*  ===================================================================
!*
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Unit testing
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  DESCRIPTION                : Testing the intrinsics with deferred
!*                               lenght characters:
!*                               1. ADJUSTL
!*                               2. ADJUSTR
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
      character*20, target :: ch_const='          ABCDEFGHIJ'
      character(:), allocatable :: ch_var
      character(:), pointer :: pchar
      integer*4 rc /0/

      pchar => ch_const

      if (ADJUSTL(pchar(1:1)) .ne. ' ') then
         rc = rc + 1
      endif

      if (ADJUSTR(pchar(1:1)) .ne. ' ') then
         rc = rc + 2
      endif

      if (ADJUSTL(ch_const(4:17)) .ne. 'ABCDEFG ') then
         rc = rc + 3
      endif

      if (ADJUSTR(pchar(4:17)) .ne. '       ABCDEFG') then
         rc = rc + 4
      endif

      allocate (character(10)::ch_var)
      ch_var = '  012345  '     ! Centre justified

      if (ADJUSTL(ch_var//'345') .ne. '012345  345') then
         rc = rc + 5
      endif

      if (ADJUSTR(ch_var//'345') .ne. '  012345  345') then
         rc = rc + 6
      endif

      if (ADJUSTL(' 222'//ch_var) .ne. '222  012345 ') then
         rc = rc + 7
      endif

      if (ADJUSTR(' 222'//ch_var) .ne. '   222  012345') then
         rc = rc + 8
      endif

      if (ADJUSTL('   '//ch_var(1:5)//'%^&') .ne. '012%^&   ') then
         rc = rc + 9
      endif

      if (ADJUSTR('   '//ch_var(1:5)//'%^&') .ne. '     012%^&') then
         rc = rc + 10
      endif

      if (ADJUSTL(ch_var(1:10)) .ne. '012345          ') then
         rc = rc + 11
      endif

      if (ADJUSTR(ch_var(1:10)) .ne. '    012345') then
         rc = rc + 12
      endif

      deallocate(ch_var)
      call zzrc(rc)

      end
