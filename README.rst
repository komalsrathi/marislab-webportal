.. |date| date::

*********************
Maris' Lab Web Portal
*********************

:authors: Komal Rathi
:contact: rathik@email.chop.edu
:organization: DBHi, CHOP
:status: This is "work in progress"
:date: |date|

.. meta::
   :keywords: web, portal, rshiny, 2016
   :description: DBHi Rshiny Web Portal.

Introduction
============

This is the Maris Lab's Web Portal Repo.

Updates
^^^^^^^

1. Creates dropdown to select between datasets.
2. Changes all textInputs to selectInputs.

	- Circumvents error when typing gene names. 
	- Only available gene names can be selected.
4. Changes all radiobuttons to either selectInputs or checkBoxInputs
	
	- Sleeker versions of radiobuttons.
5. Adds log checkbox to various plots to get output in log or delogged format.
6. Generates plots in plotly.
7. Allows users to choose between correlation types.
8. More to come later!

Improvements required
^^^^^^^^^^^^^^^^^^^^^

1. lock gene and symbol fields (cannot test on OSX)
2. add description for each project
3. for a plot, get raw data from which the plot came
4. add functionality for b/w plots
