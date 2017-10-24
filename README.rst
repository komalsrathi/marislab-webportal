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

Updates from old version
^^^^^^^^^^^^^^^^^^^^^^^^

* Creates dropdown to select between datasets.
* Changes all textInputs to selectInputs.
  
  - Circumvents error when typing gene names. 
  - Only available gene names can be selected.
	
* Changes all radiobuttons to either selectInputs or checkBoxInputs.

  - Sleeker versions of radiobuttons.
  
* Adds log checkbox to various plots to get output in log or delogged format.
* Generates interactive plots in plotly.
* Allows users to choose between correlation types.
* Fixes gene names in patient data (messed up in Excel).
* Adds Kallisto TPM data.
* Adds data summary.
* Locks reference columns for large dataframes.
* Removes non-informative rownames.
* Adds transparency to density plots.
* Adds ~15 datasets from R2/GEO/ArrayExpress.
* Updates patient data dropdown using colnames in metadata.
* Replaces Tabletools with Buttons extension.
* added functionality for b/w plots using dropdown.
* added description for each project.

Improvements required (long term)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

* user traffic record

What's new
^^^^^^^^^^

* PDX MYCN status has been updated	
* Reprocessed PDX data with mouse substraction 
* Cell line and PDX comparisons

