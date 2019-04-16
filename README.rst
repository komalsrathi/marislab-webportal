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

For developers:
==============

# Cell line data formatting

- the names should have TPM, FPKM or RMA
- rownames of metadata should match colnames of the data files
- metadata should have a column called "CellLine". This should be same as the colnames of the data files.
