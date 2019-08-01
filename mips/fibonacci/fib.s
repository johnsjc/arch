## Author: Jc J
## Date: July 29, 2019
##
## Implementations for fibonacci algorithms
##

.globl fib.recursion
.globl fib.tail_recursion
.globl fib.iteration

.data

memo:				.align 2
					.space 188					# space for up to fib(46)
												# anything larger is an integer overflow

.text

## recursion
##
## Computes the nth fibonacci number using recursion.
##
## Arguments
##		a0:		n
##
fib.recursion:

		bgt			$a0, 0, __r_n_nonzero
		
	__r_n_zero:
	
		li			$v0, 0                          # return 0 if n = 0
		jr			$ra
		
	__r_n_nonzero:

		bgt			$a0, 1, __r_check_memoized		# return 1 if n < 2
		li			$v0, 1							
		jr			$ra
	
	__r_check_memoized:
	
		subu		$sp, $sp, 32
		sw			$ra, 28($sp)		
		sw			$a0, 24($sp)					# preserve n
	
		move		$t0, $a0						# t0: offset into memo array				
		mul			$t0, $t0, 4						# n * 4 bytes																	
														
		la			$t1, memo						# t1: memo[n]
		addu		$t1, $t1, $t0
		
		lw			$v1, ($t1)						# v1: *memo[n]
		beqz		$v1, __r						# if no memo exists, compute as normal
													
		move		$a1, $v1
		jal			debug.memo
		
		move		$v0, $v1						# else return memo
			
		lw			$ra, 28($sp)
		addu		$sp, $sp, 32
		jr			$ra								
	
	__r:

		lw			$t0, 24($sp)		
		sub			$a0, $t0, 2						# fib(n-2)
		jal			fib.recursion		
		move		$t1, $v0
		sw			$t1, 20($sp)					

		lw			$t0, 24($sp)					# fib(n-1)
        sub			$a0, $t0, 1		
		jal			fib.recursion        
		move		$t2, $v0		
		
		lw			$t1, 20($sp)					# return fib(n - 2) + fib(n - 1)		
		add			$v0, $t1, $t2
		
	__r_memoize:
		
		lw			$t0, 24($sp)					# t0: offset into memo array
		mul			$t0, $t0, 4						# n * 4 bytes
		
		la			$t1, memo						# t1: memo[n]
		addu		$t1, $t1, $t0	
		
		sw			$v0, ($t1)						# memo[n] = fib(n)		
		
	__r_end:
		
		lw			$ra, 28($sp)
		addu		$sp, $sp, 32
		jr			$ra	
## end of recursion


## tail_recursion
##
## API function to compute the nth fibonacci number using tail recursion.
## Sets up the call to __do_tail_recursion
##
## Arguments
##		a0:		n
##
fib.tail_recursion:
		subu		$sp, $sp, 32
		sw			$ra, 28($sp)
		
		li			$a1, 0							# fib(n, a=0, b=1)
		li			$a2, 1
		jal			__do_tail_recursion

		lw			$ra, 28($sp)
		addu		$sp, $sp, 32
		jr			$ra
## end of tail_recursion


## __do_tail_recursion
##
## Computes the nth fibonacci number using tail recursion.
##
## Arguments
##		a0:		n
##		a1:		a
##		a2:		b
##
__do_tail_recursion:

		bgt			$a0, 0, __tr_n_nonzero
		
	__tr_n_zero:

		move        $v0, $a1                        # return a if n = 0
		jr			$ra

	__tr_n_nonzero:

		bgt			$a0, 1, __tr					# return b if n < 2
		move		$v0, $a2						
		jr			$ra
		
	__tr:
	
		subu		$sp, $sp, 32
		sw			$ra, 28($sp)
		
		sub			$a0, $a0, 1						# 1st arg: n - 1
		
		move		$t0, $a2						# t0: a + b
		add			$t0, $a2, $a1
		
		move		$a1, $a2						# 2nd arg: b
		move		$a2, $t0						# 3rd arg: a + b
		
		jal			__do_tail_recursion				# fib(n - 1, b, a+b)
		
		lw			$ra, 28($sp)
		addu		$sp, $sp, 32
		jr			$ra
## end of __do_tail_recursion


## iteration
##
## Computes the nth fibonacci number using iteration.
##
## Arguments
##		a0:		n
##
fib.iteration:

		bgt			$a0, 0, __i_n_nonzero		
		
	__i_n_zero:
	
		li			$v0, 0							# return 0 if n = 0
		jr			$ra
				
	__i_n_nonzero:

		bgt			$a0, 1, __i						# return 1 if n < 2
		li			$v0, 1
		jr			$ra
		
	__i:
	
		li			$t0, 0							# a = 1
		li			$t1, 1							# b = 1
		
		subi		$a0, $a0, 1						# use n as loop counter
		
	__i_loop:
	
		beqz		$a0, __i_end
		add			$t2, $t0, $t1					# c = a + b
		move		$t0, $t1						# a = b
		move		$t1, $t2						# b = c
		subi		$a0, $a0, 1						# n--
		b			__i_loop
		
	__i_end:
	
		move		$v0, $t2						# return c		
		jr			$ra
## end of iteration
