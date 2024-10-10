module saxpymod
   use iso_fortran_env
   use omp_lib
  
   implicit none
   private

   public :: saxpy,initialize

contains
        
subroutine saxpy(a, x, y, n)
   implicit none
   integer,intent(in) :: n
   integer :: i
   real(kind=real32) :: a
   real(kind=real32), dimension(:),allocatable,intent(in) :: x
   real(kind=real32), dimension(:),allocatable,intent(inout) :: y
   real(kind=real64) :: start, finish

   !NBEDIT
   logical :: is_initial_device
   integer :: num_devices
   integer :: device_id
   !is_initial_device = omp_is_initial_device()
   !NBEDIT

   num_devices = omp_get_num_devices()
   start = OMP_GET_WTIME()
   !$omp target data map(to:x,n), map(tofrom:y), map(from:is_initial_device)
   device_id = omp_get_device_num()
   is_initial_device = omp_is_initial_device()
   !$omp target teams distribute parallel do
   do i=1,n
       y(i) = a * x(i) + y(i)
   end do
   !$omp end target teams distribute parallel do
   !$omp end target data

   write (*, *) 'WAS INITIAL DEVICE ABOVE? : ', is_initial_device
   write (*, *) 'IS INITIAL DEVICE HERE? : ', omp_is_initial_device()
   write (*, *) 'NUMBER OF DEVICES : ', num_devices
   write (*, *) 'DEVICE ID : ', device_id

   finish = OMP_GET_WTIME()
   write (*, '("Time of kernel: ",f8.6)') finish-start

   write(*,*) "plausibility check:"
   write(*,*) "y(1)",y(1)
   write(*,*) "y(n-1)",y(n-1)
end subroutine saxpy

subroutine initialize(x,y,n)
   implicit none 

   integer,intent(in) :: n
   integer :: i
   real(kind=real32), dimension(:),allocatable,intent(inout) :: x
   real(kind=real32), dimension(:),allocatable,intent(inout) :: y

   do i=1,n
     x(i) = 1.0_real32
     y(i) = 2.0_real32
   end do
   end subroutine initialize

end module saxpymod

program main

   use iso_fortran_env
   use saxpymod, ONLY:saxpy,initialize
   implicit none

   integer,parameter :: n = 10000000
   real(kind=real32), allocatable, dimension(:) :: x
   real(kind=real32), allocatable, dimension(:) :: y
   real(kind=real32) :: a

   allocate(x(1:n))
   allocate(y(1:n))

   call initialize(x,y,n)
   a = 2.0_real32

   call saxpy(a, x, y, n)

end program main
