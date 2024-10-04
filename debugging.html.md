---
title: "How Do Household Energy Transitions Work?"
author:
  - name: Jill Baumgartner (Co-PI)
    affil-id: 1,2
  - name: Sam Harper (Co-PI)
    affil-id: 1,2,3,4

affiliations: 
  - id: 1
    name: McGill University
  - id: 2
    name: University of Hummingbirds
  - id: 3
    name: Erasmus University
  - id: 4
    name: Unversity of Copenhagen

date: today

format: 
 html:
   keep-md: true

# format:
#  docx: 
#    toc: true
#    number-sections: true
#    number-depth: 3

#format:
# pdf:
#   template-partials:
#      - title.tex
#   toc: true
#   keep-tex: true
#   number-sections: true
#   number-depth: 3
#   include-in-header:
#      - text: |
#         \usepackage[noblocks]{authblk}
#         \renewcommand*{\Authsep}{, }
#         \renewcommand*{\Authand}{, }
#         \renewcommand*{\Authands}{, }
#         \renewcommand\Affilfont{\small}
#         \usepackage{wrapfig}
#         \usepackage{colortbl}
#          \makeatletter
#          \renewenvironment{table}%
#            {\renewcommand\familydefault\sfdefault
#             \@float{table}}
#            {\end@float}
#          \renewenvironment{figure}%
#            {\renewcommand\familydefault\sfdefault
#             \@float{figure}}
#            {\end@float}
#          \makeatother
#         \usepackage{changes}
#   geometry: 
#      - right=1in
#      - left=1in
#   fig-pos: H
bibliography: hei-report.bib
csl: environmental-health-perspectives.csl
---


::: {.cell}

:::





\newpage

# Introduction

China is deploying an ambitious policy to transition up to 70% of households in northern China from residential coal heating to electric or gas “clean” space heating, including a large-scale roll out across rural and peri-urban Beijing, referred to in this document as China’s Coal Ban and Heat Pump (CBHP) subsidy policy. To meet this target the Beijing municipal government announced a two-pronged program that designates coal-restricted areas and simultaneously offers subsidies to night-time electricity rates and for the purchase and installation of electric-powered heat pumps to replace traditional coal-heating stoves. The policy was piloted in 2015 and, starting in 2016, was rolled out on a village-by-village basis. The variability in when the policy was applied to each village allowed us to treat the roll-out of the program as a quasi-randomized intervention and evaluate its impacts on air quality and health. Household air pollution is a well-established risk factor for adverse health outcomes over the entire lifecourse, yet there is no consensus that clean energy interventions can improve these health outcomes based on evidence from randomized trials [@lai2024]. Households may be differentially affected by the CBHP due to factors such as financial constraints and user preferences, and there is uncertainty about whether and how the policy may affect indoor and outdoor air pollution, as well as heating behaviors and health outcomes.

## Subheading

\newpage

## Description of study sample



::: {#tbl-a-fe .cell tbl-cap='Effects of the CBHP policy on personal exposure ($\mu g / m^{3}$) with variations in fixed effects for treatment group and time.' tbl-pos='H'}
::: {.cell-output-display}


```{=html}
<!-- preamble start -->

    <script>
      function styleCell_uq2wvg2655233zyfdk55(i, j, css_id) {
        var table = document.getElementById("tinytable_uq2wvg2655233zyfdk55");
        table.rows[i].cells[j].classList.add(css_id);
      }
      function insertSpanRow_1eonylkdve9afyxoro5l(i, colspan, content) {
        var table = document.getElementById('tinytable_uq2wvg2655233zyfdk55');
        var newRow = table.insertRow(i);
        var newCell = newRow.insertCell(0);
        newCell.setAttribute("colspan", colspan);
        // newCell.innerText = content;
        // this may be unsafe, but innerText does not interpret <br>
        newCell.innerHTML = content;
      }
      function spanCell_uq2wvg2655233zyfdk55(i, j, rowspan, colspan) {
        var table = document.getElementById("tinytable_uq2wvg2655233zyfdk55");
        const targetRow = table.rows[i];
        const targetCell = targetRow.cells[j];
        for (let r = 0; r < rowspan; r++) {
          // Only start deleting cells to the right for the first row (r == 0)
          if (r === 0) {
            // Delete cells to the right of the target cell in the first row
            for (let c = colspan - 1; c > 0; c--) {
              if (table.rows[i + r].cells[j + c]) {
                table.rows[i + r].deleteCell(j + c);
              }
            }
          }
          // For rows below the first, delete starting from the target column
          if (r > 0) {
            for (let c = colspan - 1; c >= 0; c--) {
              if (table.rows[i + r] && table.rows[i + r].cells[j]) {
                table.rows[i + r].deleteCell(j);
              }
            }
          }
        }
        // Set rowspan and colspan of the target cell
        targetCell.rowSpan = rowspan;
        targetCell.colSpan = colspan;
      }
window.addEventListener('load', function () { insertSpanRow_1eonylkdve9afyxoro5l(14, 13, 'Heating season') });
window.addEventListener('load', function () { insertSpanRow_1eonylkdve9afyxoro5l(10, 13, 'Daytime heating') });
window.addEventListener('load', function () { insertSpanRow_1eonylkdve9afyxoro5l(6, 13, 'Daytime') });
window.addEventListener('load', function () { insertSpanRow_1eonylkdve9afyxoro5l(2, 13, 'All times') });
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(0, 0, 'tinytable_css_idha6z63dix2naliyg2nm6') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(0, 1, 'tinytable_css_idksdn0lmqtazlzxe99r55') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(0, 2, 'tinytable_css_idksdn0lmqtazlzxe99r55') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(0, 3, 'tinytable_css_idksdn0lmqtazlzxe99r55') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(0, 4, 'tinytable_css_id7og5ah1r532bwkqgz1xw') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(0, 5, 'tinytable_css_id7og5ah1r532bwkqgz1xw') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(0, 6, 'tinytable_css_id7og5ah1r532bwkqgz1xw') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(0, 7, 'tinytable_css_id7og5ah1r532bwkqgz1xw') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(0, 8, 'tinytable_css_id7og5ah1r532bwkqgz1xw') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(0, 9, 'tinytable_css_id7og5ah1r532bwkqgz1xw') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(0, 10, 'tinytable_css_id7og5ah1r532bwkqgz1xw') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(0, 11, 'tinytable_css_id7og5ah1r532bwkqgz1xw') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(0, 12, 'tinytable_css_id7og5ah1r532bwkqgz1xw') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(1, 0, 'tinytable_css_idppaobyl088ptnaha0qkn') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(1, 1, 'tinytable_css_idkope8v2db2v50hausdlr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(1, 2, 'tinytable_css_idkope8v2db2v50hausdlr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(1, 3, 'tinytable_css_idkope8v2db2v50hausdlr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(1, 4, 'tinytable_css_idkope8v2db2v50hausdlr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(1, 5, 'tinytable_css_idkope8v2db2v50hausdlr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(1, 6, 'tinytable_css_idkope8v2db2v50hausdlr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(1, 7, 'tinytable_css_idkope8v2db2v50hausdlr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(1, 8, 'tinytable_css_idkope8v2db2v50hausdlr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(1, 9, 'tinytable_css_idkope8v2db2v50hausdlr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(1, 10, 'tinytable_css_idkope8v2db2v50hausdlr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(1, 11, 'tinytable_css_idkope8v2db2v50hausdlr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(1, 12, 'tinytable_css_idkope8v2db2v50hausdlr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(2, 0, 'tinytable_css_idap38wllq6vfaazvd98pq') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(2, 1, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(2, 2, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(2, 3, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(2, 4, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(2, 5, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(2, 6, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(2, 7, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(2, 8, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(2, 9, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(2, 10, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(2, 11, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(2, 12, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(3, 0, 'tinytable_css_idtf85it91crezl6ul5nsl') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(3, 1, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(3, 2, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(3, 3, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(3, 4, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(3, 5, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(3, 6, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(3, 7, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(3, 8, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(3, 9, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(3, 10, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(3, 11, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(3, 12, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(4, 0, 'tinytable_css_idtf85it91crezl6ul5nsl') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(4, 1, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(4, 2, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(4, 3, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(4, 4, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(4, 5, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(4, 6, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(4, 7, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(4, 8, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(4, 9, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(4, 10, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(4, 11, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(4, 12, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(5, 0, 'tinytable_css_idtf85it91crezl6ul5nsl') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(5, 1, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(5, 2, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(5, 3, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(5, 4, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(5, 5, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(5, 6, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(5, 7, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(5, 8, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(5, 9, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(5, 10, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(5, 11, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(5, 12, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(6, 0, 'tinytable_css_idtf85it91crezl6ul5nsl') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(6, 1, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(6, 2, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(6, 3, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(6, 4, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(6, 5, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(6, 6, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(6, 7, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(6, 8, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(6, 9, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(6, 10, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(6, 11, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(6, 12, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(7, 0, 'tinytable_css_idap38wllq6vfaazvd98pq') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(7, 1, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(7, 2, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(7, 3, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(7, 4, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(7, 5, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(7, 6, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(7, 7, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(7, 8, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(7, 9, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(7, 10, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(7, 11, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(7, 12, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(8, 0, 'tinytable_css_idtf85it91crezl6ul5nsl') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(8, 1, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(8, 2, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(8, 3, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(8, 4, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(8, 5, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(8, 6, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(8, 7, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(8, 8, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(8, 9, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(8, 10, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(8, 11, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(8, 12, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(9, 0, 'tinytable_css_idtf85it91crezl6ul5nsl') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(9, 1, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(9, 2, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(9, 3, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(9, 4, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(9, 5, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(9, 6, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(9, 7, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(9, 8, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(9, 9, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(9, 10, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(9, 11, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(9, 12, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(10, 0, 'tinytable_css_idtf85it91crezl6ul5nsl') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(10, 1, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(10, 2, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(10, 3, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(10, 4, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(10, 5, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(10, 6, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(10, 7, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(10, 8, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(10, 9, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(10, 10, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(10, 11, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(10, 12, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(11, 0, 'tinytable_css_idtf85it91crezl6ul5nsl') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(11, 1, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(11, 2, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(11, 3, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(11, 4, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(11, 5, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(11, 6, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(11, 7, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(11, 8, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(11, 9, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(11, 10, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(11, 11, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(11, 12, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(12, 0, 'tinytable_css_idap38wllq6vfaazvd98pq') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(12, 1, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(12, 2, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(12, 3, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(12, 4, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(12, 5, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(12, 6, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(12, 7, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(12, 8, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(12, 9, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(12, 10, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(12, 11, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(12, 12, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(13, 0, 'tinytable_css_idtf85it91crezl6ul5nsl') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(13, 1, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(13, 2, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(13, 3, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(13, 4, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(13, 5, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(13, 6, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(13, 7, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(13, 8, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(13, 9, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(13, 10, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(13, 11, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(13, 12, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(14, 0, 'tinytable_css_idtf85it91crezl6ul5nsl') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(14, 1, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(14, 2, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(14, 3, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(14, 4, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(14, 5, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(14, 6, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(14, 7, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(14, 8, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(14, 9, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(14, 10, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(14, 11, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(14, 12, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(15, 0, 'tinytable_css_idtf85it91crezl6ul5nsl') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(15, 1, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(15, 2, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(15, 3, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(15, 4, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(15, 5, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(15, 6, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(15, 7, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(15, 8, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(15, 9, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(15, 10, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(15, 11, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(15, 12, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(16, 0, 'tinytable_css_idtf85it91crezl6ul5nsl') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(16, 1, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(16, 2, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(16, 3, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(16, 4, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(16, 5, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(16, 6, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(16, 7, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(16, 8, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(16, 9, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(16, 10, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(16, 11, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(16, 12, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(17, 0, 'tinytable_css_idap38wllq6vfaazvd98pq') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(17, 1, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(17, 2, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(17, 3, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(17, 4, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(17, 5, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(17, 6, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(17, 7, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(17, 8, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(17, 9, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(17, 10, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(17, 11, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(17, 12, 'tinytable_css_idf37njjwokmn2r97w7rzs') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(18, 0, 'tinytable_css_idtf85it91crezl6ul5nsl') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(18, 1, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(18, 2, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(18, 3, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(18, 4, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(18, 5, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(18, 6, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(18, 7, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(18, 8, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(18, 9, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(18, 10, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(18, 11, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(18, 12, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(19, 0, 'tinytable_css_idtf85it91crezl6ul5nsl') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(19, 1, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(19, 2, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(19, 3, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(19, 4, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(19, 5, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(19, 6, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(19, 7, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(19, 8, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(19, 9, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(19, 10, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(19, 11, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(19, 12, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(20, 0, 'tinytable_css_idtf85it91crezl6ul5nsl') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(20, 1, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(20, 2, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(20, 3, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(20, 4, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(20, 5, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(20, 6, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(20, 7, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(20, 8, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(20, 9, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(20, 10, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(20, 11, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(20, 12, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(21, 0, 'tinytable_css_idtf85it91crezl6ul5nsl') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(21, 1, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(21, 2, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(21, 3, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(21, 4, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(21, 5, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(21, 6, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(21, 7, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(21, 8, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(21, 9, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(21, 10, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(21, 11, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(21, 12, 'tinytable_css_id41s6r8zbsf7vi58nneyr') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(22, 0, 'tinytable_css_idsp2tqya37hahg34rrnez') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(22, 1, 'tinytable_css_idsp2tqya37hahg34rrnez') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(22, 2, 'tinytable_css_idsp2tqya37hahg34rrnez') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(22, 3, 'tinytable_css_idsp2tqya37hahg34rrnez') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(22, 4, 'tinytable_css_idsp2tqya37hahg34rrnez') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(22, 5, 'tinytable_css_idsp2tqya37hahg34rrnez') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(22, 6, 'tinytable_css_idsp2tqya37hahg34rrnez') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(22, 7, 'tinytable_css_idsp2tqya37hahg34rrnez') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(22, 8, 'tinytable_css_idsp2tqya37hahg34rrnez') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(22, 9, 'tinytable_css_idsp2tqya37hahg34rrnez') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(22, 10, 'tinytable_css_idsp2tqya37hahg34rrnez') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(22, 11, 'tinytable_css_idsp2tqya37hahg34rrnez') })
window.addEventListener('load', function () { styleCell_uq2wvg2655233zyfdk55(22, 12, 'tinytable_css_idsp2tqya37hahg34rrnez') })
    </script>

    <style>
    .table td.tinytable_css_idha6z63dix2naliyg2nm6, .table th.tinytable_css_idha6z63dix2naliyg2nm6 {  text-align: left;  font-size: 0.8em;  text-align: center; }
    .table td.tinytable_css_idksdn0lmqtazlzxe99r55, .table th.tinytable_css_idksdn0lmqtazlzxe99r55 {  font-size: 0.8em;  text-align: center;  border-bottom: solid 0.05em #d3d8dc; }
    .table td.tinytable_css_id7og5ah1r532bwkqgz1xw, .table th.tinytable_css_id7og5ah1r532bwkqgz1xw {  font-size: 0.8em;  text-align: center; }
    .table td.tinytable_css_idppaobyl088ptnaha0qkn, .table th.tinytable_css_idppaobyl088ptnaha0qkn {  text-align: left;  font-size: 0.8em;  border-bottom: solid 0.1em #d3d8dc; }
    .table td.tinytable_css_idkope8v2db2v50hausdlr, .table th.tinytable_css_idkope8v2db2v50hausdlr {  font-size: 0.8em;  border-bottom: solid 0.1em #d3d8dc; }
    .table td.tinytable_css_idap38wllq6vfaazvd98pq, .table th.tinytable_css_idap38wllq6vfaazvd98pq {  text-align: left;  font-size: 0.8em;  font-weight: bold; text-align: left; }
    .table td.tinytable_css_idf37njjwokmn2r97w7rzs, .table th.tinytable_css_idf37njjwokmn2r97w7rzs {  font-size: 0.8em;  font-weight: bold; text-align: left; }
    .table td.tinytable_css_idtf85it91crezl6ul5nsl, .table th.tinytable_css_idtf85it91crezl6ul5nsl {  text-align: left;  font-size: 0.8em; padding-left: 1em; }
    .table td.tinytable_css_id41s6r8zbsf7vi58nneyr, .table th.tinytable_css_id41s6r8zbsf7vi58nneyr {  font-size: 0.8em; }
    .table td.tinytable_css_idsp2tqya37hahg34rrnez, .table th.tinytable_css_idsp2tqya37hahg34rrnez {  text-align: left; }
    </style>
    <div class="container">
      <table class="table table-borderless" id="tinytable_uq2wvg2655233zyfdk55" style="width: auto; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        <thead>
<tr>
<th scope="col" align="center" colspan=1> </th>
<th scope="col" align="center" colspan=4>Point temp (°C)</th>
<th scope="col" align="center" colspan=4>Mean temp (°C)</th>
<th scope="col" align="center" colspan=4>Min temp (°C)</th>
</tr>
        
              <tr>
                <th scope="col">Cohort Time</th>
                <th scope="col">ATT</th>
                <th scope="col">(95%CI)</th>
                <th scope="col">p<sup>a</sup></th>
                <th scope="col">Obs</th>
                <th scope="col">ATT</th>
                <th scope="col">(95%CI)</th>
                <th scope="col">p<sup>a</sup></th>
                <th scope="col">Obs</th>
                <th scope="col">ATT</th>
                <th scope="col">(95% CI)</th>
                <th scope="col">p<sup>a</sup></th>
                <th scope="col">Obs</th>
              </tr>
        </thead>
        <tfoot><tr><td colspan='13'><sup></sup> Note: ATT = Average Treatment Effect on the Treated, CI = confidence interval</td></tr>
<tr><td colspan='13'><sup>a</sup> P-value for omnibus test of heterogeneity across cohort time groups.</td></tr></tfoot>
        <tbody>
                <tr>
                  <td>2019 2019</td>
                  <td>1.66</td>
                  <td>(0.5, 2.8)</td>
                  <td></td>
                  <td></td>
                  <td>0.33</td>
                  <td>(-0.8, 1.5)</td>
                  <td></td>
                  <td></td>
                  <td>1.96</td>
                  <td>(0.5, 3.4)</td>
                  <td></td>
                  <td></td>
                </tr>
                <tr>
                  <td>2019 2021</td>
                  <td>2.17</td>
                  <td>(0.5, 3.9)</td>
                  <td></td>
                  <td></td>
                  <td>0.80</td>
                  <td>(0.0, 1.6)</td>
                  <td></td>
                  <td></td>
                  <td>5.04</td>
                  <td>(2.3, 7.8)</td>
                  <td></td>
                  <td></td>
                </tr>
                <tr>
                  <td>2020 2021</td>
                  <td>2.39</td>
                  <td>(0.7, 4.1)</td>
                  <td></td>
                  <td></td>
                  <td>0.66</td>
                  <td>(-0.5, 1.8)</td>
                  <td></td>
                  <td></td>
                  <td>7.27</td>
                  <td>(4.6, 9.9)</td>
                  <td></td>
                  <td></td>
                </tr>
                <tr>
                  <td>2021 2021</td>
                  <td>0.60</td>
                  <td>(-1.2, 2.4)</td>
                  <td>0.37</td>
                  <td>2999</td>
                  <td>1.60</td>
                  <td>(0.5, 2.7)</td>
                  <td>0.45</td>
                  <td>1350</td>
                  <td>2.37</td>
                  <td>(0.2, 4.5)</td>
                  <td>0</td>
                  <td>1350</td>
                </tr>
                <tr>
                  <td>2019 2019</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td>0.36</td>
                  <td>(-0.8, 1.5)</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                </tr>
                <tr>
                  <td>2019 2021</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td>0.91</td>
                  <td>(0.1, 1.7)</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                </tr>
                <tr>
                  <td>2020 2021</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td>0.95</td>
                  <td>(-0.2, 2.1)</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                </tr>
                <tr>
                  <td>2021 2021</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td>1.67</td>
                  <td>(0.5, 2.8)</td>
                  <td>0.48</td>
                  <td>1346</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                </tr>
                <tr>
                  <td>2019 2019</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td>0.92</td>
                  <td>(-0.2, 2.0)</td>
                  <td></td>
                  <td></td>
                  <td>1.94</td>
                  <td>(0.5, 3.4)</td>
                  <td></td>
                  <td></td>
                </tr>
                <tr>
                  <td>2019 2021</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td>2.02</td>
                  <td>(0.9, 3.2)</td>
                  <td></td>
                  <td></td>
                  <td>5.46</td>
                  <td>(2.7, 8.2)</td>
                  <td></td>
                  <td></td>
                </tr>
                <tr>
                  <td>2020 2021</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td>2.63</td>
                  <td>(1.5, 3.7)</td>
                  <td></td>
                  <td></td>
                  <td>6.69</td>
                  <td>(4.2, 9.2)</td>
                  <td></td>
                  <td></td>
                </tr>
                <tr>
                  <td>2021 2021</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td>2.72</td>
                  <td>(0.8, 4.6)</td>
                  <td>0</td>
                  <td>1350</td>
                  <td>2.53</td>
                  <td>(0.4, 4.7)</td>
                  <td>0</td>
                  <td>1350</td>
                </tr>
                <tr>
                  <td>2019 2019</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td>0.95</td>
                  <td>(-0.2, 2.1)</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                </tr>
                <tr>
                  <td>2019 2021</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td>2.18</td>
                  <td>(0.9, 3.4)</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                </tr>
                <tr>
                  <td>2020 2021</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td>2.97</td>
                  <td>(1.9, 4.0)</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                </tr>
                <tr>
                  <td>2021 2021</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td>2.80</td>
                  <td>(0.9, 4.7)</td>
                  <td>0</td>
                  <td>1346</td>
                  <td></td>
                  <td></td>
                  <td></td>
                  <td></td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```


:::
:::



\newpage
