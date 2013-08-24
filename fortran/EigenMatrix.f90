!Copyright (C) 2007 Ashwith Jerome Rego
!
!This program is free software: you can redistribute it and/or modify
!it under the terms of the GNU General Public License as published by
!the Free Software Foundation, either version 3 of the License, or
!(at your option) any later version.
!
!This program is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.
!
!You should have received a copy of the GNU General Public License
!along with this program.  If not, see <http://www.gnu.org/licenses/>.


PROGRAM EIGEN_MAT
  IMPLICIT NONE
  REAL, ALLOCATABLE, DIMENSION(:,:) :: Matrix
  REAL, ALLOCATABLE, DIMENSION(:) :: x
  REAL :: EigenVal
  INTEGER :: n, i, j, steps
  
  PRINT*,"Enter order of matrix"
  READ*,n
  
  ALLOCATE(Matrix(n,n))
  ALLOCATE(x(n))
  
  PRINT*,"Enter elements row-wise"
  DO i = 1,n
    READ*,(Matrix(i,j),j=1,n)
  END DO
  
  PRINT*,"Enter number of iterations"
  READ*,steps
  
  CALL FINDEIGEN(Matrix, n, x, EigenVal, steps)
  
  PRINT*,"The largest eigenvalue of the matrix is ",EigenVal
  PRINT*,"The largest eigenvector of the matrix is:"
  DO i = 1,n
    PRINT*,x(i)
  END DO
END PROGRAM EIGEN_MAT

!Author : Louisda16th a.k.a Ashwith J. Rego
!These set of subroutines find the largest eigenvalue and eigenmatrix of the matrix.
!The algorithm is based on Rayleigh's power method
!Information available at :
!Please note that the subroutine used to multiply the two matrices is not general.
!Also note that the number of iterations must be specified
SUBROUTINE FINDEIGEN(Matrix, n, x, EigenVal, steps)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n, steps  !n = order of matrix, steps = number of iterations
  REAL, INTENT(IN), DIMENSION(n,n) :: Matrix(n,n)  !Input Matrix
  REAL, INTENT(INOUT), DIMENSION(n) :: x !Eigenvector
  REAL, INTENT(INOUT) :: EigenVal !Eigenvalue
  INTEGER :: i, j
  
  x  = 1 !Initialize eigen vector to any value.
  
  DO i = 1, steps
    CALL MULMATRIX(Matrix, x, n)       !Multiply input matrix by eigenvector
    CALL FINDLARGEST(x, n, EigenVal)   !Find eigenvalue
    IF(EigenVal == 0) EXIT      
    DO j = 1, n                        !Find eigenvector
      x(j) = x(j)/EigenVal
    END DO  
  END DO

END SUBROUTINE FINDEIGEN

SUBROUTINE MULMATRIX(a, b, n)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n !matrix size
  REAL, INTENT(IN), DIMENSION(n,n) :: a  !Matrix of order > 1
  REAL, INTENT(INOUT), DIMENSION(n) :: b !1x1 matrix
  
  INTEGER i, j
  REAL, DIMENSION(n) :: temp !temporary matrix

  temp = 0
  
  !These two loops to the multiplication
  DO i = 1, n
    DO j = 1, n
      temp(i) = temp(i) + a(i,j)*b(j)
    END DO
  END DO
  b = temp

END SUBROUTINE MULMATRIX

SUBROUTINE FINDLARGEST(x, n, l)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  REAL, INTENT(IN), DIMENSION(n) :: x
  REAL, INTENT(INOUT) :: l !Largest value
  
  INTEGER :: i
  !Algorithm is easy
  !Let the largest number be the first one.
  !If you find a number larger than it, store this number and then continue
  l = ABS(x(1))
  DO i = 2, n
    IF (ABS(x(i)) > l) l = ABS(x(i))
  END DO
    
END SUBROUTINE FINDLARGEST
