!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 22, 2007
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ENCODING= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Functional testing of I/O using ENCODING= specifier
!*
!234567890123456789012345678901234567890123456789012345678901234567890
program encodingspecifier011
  integer caseid
  integer ire,ireca(2)
  integer iarr1(2),ivar1
  character crvar1,cwvar1
  character crarr1(3)
  character(20) which_mode
  character*5 crva1(3)
  parameter (isymb = 1)
  irecf(I) = I
  open(01,access='direct',blank='zero',recl=150,form='formatted',encoding='DEFAULT')
  caseid = 001
  crvar1 = 'A'
  cwvar1 = 'B'
  read (1,iostat=ios,fmt=121,rec=1) crvar1
121 format(1a)
  if (crvar1.ne.'B' .or. IOS.ne.0) call zzrc(caseid)
  caseid = 002
  crarr1(1) = 'A'
  crarr1(2) = 'B'
  crarr1(3) = 'C'
  assign 224 to lfmt
224 format(a2,a2,a2)
  inquire(1,nextrec=irec, encoding=which_mode)
  ire=irec
  read (isymb,iostat=ios,fmt=lfmt,rec=ire) crarr1
  if (which_mode.ne.'DEFAULT' .or. crarr1(1).ne.'D' .or. crarr1(2).ne.'E' .or. crarr1(3).ne.'F' .or. ios.ne.0)  call zzrc(caseid)
  CASEID = 003
  CRARR1(1) = 'S'
  CRARR1(2) = 'D'
  CRARR1(3) = 'G'
  IVAR1 = 1
  INQUIRE(1,NEXTREC=IREC,encoding=which_mode)
  IRECA(1)=IREC
  READ (IVAR1,IOSTAT=IOS,FMT='(3A1)',REC=IRECA(1)) CRARR1
  IF (CRARR1(1).NE.'E'.OR.CRARR1(2).NE.'W'.OR.CRARR1(3).NE.'X'.OR.IOS.NE.0 ) CALL ZZRC(CASEID)
  CASEID = 004
  CRVA1 = '(1A3)'
  IARR1(1) = 1
  INQUIRE(1,NEXTREC=IREC)
  IRE=IREC
  READ (IARR1(1),IOSTAT=IOS,FMT=CRVA1,REC=IRECF(IRE))CRVAR1
  IF (CRVAR1.NE.'Z' .OR.IOS.NE.0 ) CALL ZZRC(CASEID)
end program encodingspecifier011
