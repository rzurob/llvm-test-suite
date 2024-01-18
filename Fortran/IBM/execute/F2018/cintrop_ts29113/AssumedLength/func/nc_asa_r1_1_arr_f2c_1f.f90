! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE     : C Interop: Assumed-length Character arguments
!*
!*
!*
!*  PROGRAMMER          : Umme Hunny
!*  DATE                : June, 1, 2014
!*  ORIGIN              : AIX Compiler Development, Toronto Lab
!*  FEATURE             : RTC Master Story:
!*                        C Interop: Assumed-length Character arguments
!*                        (master story) (72333)
!*
!*  FEATURE             : C Interop: Assumed-length Character arguments 
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012          

      program assumed_lenght001
        interface
          subroutine asar1_1(c_arg1, i_len, ext) bind(c)
            USE, INTRINSIC :: ISO_C_BINDING
            character(*), DIMENSION(*) :: c_arg1
            integer(C_INT) i_len, ext
          end subroutine 
        end interface

        ! Explicit Shape array 

          character(3), dimension(1) :: v_arg1 
          character(4), dimension(1,1) :: v_arg2
          character(5), dimension(10) :: v_arg3
          character(6), dimension(5,5) :: v_arg4
          character(7), dimension(1:5, -5:1, 10) :: v_arg5
          character(8), STATIC :: v_arg6(10,10)
          character(9), VOLATILE :: v_arg7(5,5,5)
          character(3), AUTOMATIC :: v_arg8(3,3)
          character(6), TARGET  :: v_arg9(2,2)
          character(6), PARAMETER :: v_arg1_2(5,5) = "ABC123"
          character(7), TARGET :: v_arg1_3(10) 


          ! Deferred Shape array
          character(6), pointer :: v_arg11(:,:)
          character(6), pointer, CONTIGUOUS :: v_arg12(:,:)
          character(:), pointer :: v_arg13(:)
          character(7), ALLOCATABLE, DIMENSION(:, :, :) :: v_arg14
          character(7), ALLOCATABLE :: v_arg15(:)
          character(:), ALLOCATABLE :: v_arg16(:,:)
          
      
          ! Implied Shape Array
          CHARACTER(5), PARAMETER :: v_arg21(4:*) = ["ABC", "DEF"]  
          CHARACTER(*), PARAMETER :: v_arg22(4:*) = ["ABCDEF", "DEFGHI"]           

          ! Explicit Shape array
          
          v_arg1(1) = "ABC"
          call asar1_1(v_arg1, 3, SIZE(v_arg1))

          v_arg2(1,1) = "ABCD"
          call asar1_1(v_arg2, 4, SIZE(v_arg2) )

          v_arg3(1:5) = "FIVES"
          
          call asar1_1(v_arg3(1:3),5, SIZE(v_arg3(1:5)))

          v_arg4(1:1,1:1) = "SIXES1"
          call asar1_1(v_arg4, 6, SIZE(v_arg4(1:1,1:1)))

          v_arg5(1:1, -5:-5, 1:1) = "SEVENS"
          call asar1_1(v_arg5, 7, SIZE(v_arg5))

          v_arg6 = "EIGHTS"
          call asar1_1(v_arg6,8, SIZE(v_arg6))

          v_arg7 = "NINES"
          call asar1_1(v_arg7, 9, SIZE(v_arg7))

          v_arg8 = "TEN"
          call asar1_1(v_arg8, 3, SIZE(v_arg8))
         
          v_arg9 = "TEST12"
          call asar1_1(v_arg9, 6, SIZE(v_arg9))

          call asar1_1(v_arg1_2,6, SIZE(v_arg1_2))

          v_arg1_3 = "TEST13"
          
          ! Deferred Shape array

          v_arg11 => v_arg9
          v_arg12 => v_arg9
          v_arg13 => v_arg1_3

          call asar1_1(v_arg11,6, SIZE(v_arg11))
          call asar1_1(v_arg12,6, SIZE(v_arg12))
          call asar1_1(v_arg13,7,SIZE(v_arg12))

          allocate(v_arg14(8, -1:3, 5))
          v_arg14 = "TEST14"
          call asar1_1(v_arg14, 7, SIZE(v_arg14))

          allocate(v_arg15(10))
          v_arg15 = "TEST15"
          call asar1_1(v_arg15, 7, SIZE(v_arg15))
          

          allocate(character(len=16) :: v_arg16(10,10))
          v_arg16 = "TEST"
          
          call asar1_1(v_arg16, 4, SIZE(v_arg16))  !defect 103037


            
           ! Implied Shape Array

          call asar1_1(v_arg21,5, SIZE(v_arg21))
          call asar1_1(v_arg22,6, SIZE(v_arg22))

          ! Explicit shape array:automatic
         
          call SUB1_1(10) 
          call SUB1_2(7) 

          ! adjustable array
 
          call SUB2_1(3, v_arg1)
          call SUB2_2(4, v_arg2)


          ! assumed shape

          call SUB3_1(v_arg14)            
          call SUB3_2(v_arg14)
          call SUB3_3(v_arg14)


          ! assumed size
          
          call SUB4_1(v_arg1,3, SIZE(v_arg1)) !defect 103255
          call SUB4_1(v_arg2,4, SIZE(v_arg2))
          call SUB4_1(v_arg3,5, SIZE(v_arg3))
          call SUB4_1(v_arg4,6, SIZE(v_arg4))
      
          call SUB4_2(v_arg1, SIZE(v_arg1))
          call SUB4_2(v_arg2, SIZE(v_arg2))
          call SUB4_2(v_arg3, SIZE(v_arg3))
          call SUB4_2(v_arg4, SIZE(v_arg4))

          call SUB4_3(v_arg5, SIZE(v_arg5))
          call SUB4_4(v_arg6, SIZE(v_arg6))
          call SUB4_5(v_arg7, 9, SIZE(v_arg7))
       

      contains
       ! Explicit shape array:
         ! automatic

       SUBROUTINE SUB1_1(Y)
         INTEGER Y
         CHARACTER(10) Z (20, 1:Y)                                            
         Z  = "SUB1_1TEST"
         call asar1_1(Z, 10, SIZE(Z))
       END SUBROUTINE
       
       SUBROUTINE SUB1_2(Y)
         INTEGER Y
         CHARACTER(Y) Z (5, 1:Y)       
         Z  = "SUB1_2TEST"
         call asar1_1(Z, 7, SIZE(Z))
       END SUBROUTINE
                    
         ! adjustable
      SUBROUTINE SUB2_1(X, Y)
         INTEGER X
         CHARACTER(10) Y(X*3)
         Y = "ADJUSTABLE"
         
        
         call asar1_1(Y, 10, SIZE(Y))
       END SUBROUTINE

       SUBROUTINE SUB2_2(X, Y)
         INTEGER X
         CHARACTER(*) Y(X*3)
         Y = "ADJUSTABLE"
         
        
         call asar1_1(Y, X, SIZE(Y) )
       END SUBROUTINE
 
         ! assumed shape 
       SUBROUTINE SUB3_1(B)
         CHARACTER(7) B(1:,:,10:)
         
         call asar1_1(B, 7, SIZE(B))
       END SUBROUTINE
       
       SUBROUTINE SUB3_2(B)
         CHARACTER(*) B(1:,:,10:)
           
           call asar1_1(B, 7, SIZE(B))
       END SUBROUTINE

 
       SUBROUTINE SUB3_3(B)
         CHARACTER(*), CONTIGUOUS ::  B(1:,:,10:)
         
         call asar1_1(B, 7, SIZE(B))
       END SUBROUTINE

         ! assumed size
       SUBROUTINE SUB4_1(ARRAY, L, D)
         CHARACTER(*) ARRAY(*)  
         integer L,D
         
         call asar1_1(ARRAY, L, D)
       END SUBROUTINE

       SUBROUTINE SUB4_2(ARRAY, D)
         CHARACTER(5) ARRAY(*)
         integer D
         
         call asar1_1(ARRAY, 5, D)

       END SUBROUTINE


       SUBROUTINE SUB4_3(ARRAY, D)
         CHARACTER(10) ARRAY(2,*)
         integer D
         
         call asar1_1(ARRAY, 10, D )
       END SUBROUTINE

       SUBROUTINE SUB4_4(ARRAY, D)
         CHARACTER(10) ARRAY(2,-4:*)
         integer D
         
         call asar1_1(ARRAY, 10, D)
       END SUBROUTINE
 
       SUBROUTINE SUB4_5(ARRAY, L, D)
         CHARACTER(*) ARRAY(2,-4:*)
         integer L, D
         
         call asar1_1(ARRAY, L, D)
       END SUBROUTINE


       end program





