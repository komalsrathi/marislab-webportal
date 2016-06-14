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

Abstract
========

`Abstract`_ for the 2016 MidAtlantic Bioinformatics Conference.

.. _Abstract: ./docs/abstract.rst

Updates
^^^^^^^

* Creates dropdown to select between datasets.
* Changes all textInputs to selectInputs.
  
  - Circumvents error when typing gene names. 
  - Only available gene names can be selected.
	
* Changes all radiobuttons to either selectInputs or checkBoxInputs.

  - Sleeker versions of radiobuttons.
  
* Adds log checkbox to various plots to get output in log or delogged format.
* Generates plots in plotly.
* Allows users to choose between correlation types.
* Fixes gene names in patient data (messed up in Excel)
* More to come later!

High-priority
^^^^^^^^^^^^^

* Visualization of data summary
* alternative to log2
* think how datatype will be decided

Improvements required
^^^^^^^^^^^^^^^^^^^^^

* lock gene and symbol fields (cannot test on OSX)
* add description for each project.
* for a plot, get raw data from which the plot came.
* add functionality for b/w plots using dropdown.
* add datatype on Y axis to show whether it is FPKM/TPM/log.
* look through plots to optimize viewing.
* user traffic record
* statistics on how many datasets/samples we have. (bubbleplot)
* check if bottom filter is visible or not.
