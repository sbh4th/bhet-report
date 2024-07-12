---
title: "How Do Household Energy Transitions Work?"
author: 
  - Jill Baumgartner (Co-PI)
  - Sam Harper (Co-PI)
  - On behalf of the Beijing Household Energy Transitions Team
date: today

format: 
  html:
    keep-md: true
#  docx: 
#    toc: true
#    number-sections: true
#    number-depth: 3

# pdf:
#   toc: true
#   keep-tex: true
#   number-sections: true
#   number-depth: 3
#   include-in-header:
#      - text: |
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




Problem table:


::: {.cell hash='debugging_cache/html/make_table1_2d7f85e5146299d2156f60633653c2fa'}

:::

::: {.cell hash='debugging_cache/html/table1_59f74a6f3c66c1885c3bba5f752347a5'}

:::


Read in the data for Table 2:

::: {.cell}
::: {.cell-output .cell-output-stderr}

```
Rows: 3672 Columns: 9
── Column specification ────────────────────────────────────────────────────────
Delimiter: ","
dbl (9): wave, ID_VILLAGE, gender_health, age_health, smoking, lived_with_sm...

ℹ Use `spec()` to retrieve the full column specification for this data.
ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```


:::
:::


Now insert the function:

::: {.cell}

:::

::: {.cell}

:::

And then make the table:

::: {.cell}
::: {.cell-output-display}


```{=html}
<!-- preamble start -->

    <script>
      function styleCell_tinytable_06xukigh80c1jrp5q689(tableId, cellCoordinates, cssClass) {
            var table = document.getElementById("tinytable_2i23uijusjbj7igeqj4w");
            cellCoordinates.forEach(([i, j]) => {
                table.rows[i].cells[j].classList.add(cssClass);
            });
        }

      function insertSpanRow(i, colspan, content) {
        var table = document.getElementById('tinytable_2i23uijusjbj7igeqj4w');
        var newRow = table.insertRow(i);
        var newCell = newRow.insertCell(0);
        newCell.setAttribute("colspan", colspan);
        // newCell.innerText = content;
        // this may be unsafe, but innerText does not interpret <br>
        newCell.innerHTML = content;
      }
      function spanCell_tinytable_06xukigh80c1jrp5q689(i, j, rowspan, colspan) {
        var table = document.getElementById("tinytable_2i23uijusjbj7igeqj4w");
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

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[1, 0], [1, 1], [1, 2], [1, 3], [1, 4], [1, 5]], class: 'tinytable_css_exn405498uollp1zsjup'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 0], [1, 0], [2, 0], [3, 0], [4, 0], [5, 0], [6, 0], [7, 0]], class: 'tinytable_css_t4if5s3u0ps5egb44p4w'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 1], [1, 1], [2, 1], [3, 1], [4, 1], [5, 1], [6, 1], [7, 1]], class: 'tinytable_css_i9uqsk3gdql67vqwuke2'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 2], [1, 2], [2, 2], [3, 2], [4, 2], [5, 2], [6, 2], [7, 2]], class: 'tinytable_css_vqmduapq1n2nfbxno0e1'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 3], [1, 3], [2, 3], [3, 3], [4, 3], [5, 3], [6, 3], [7, 3]], class: 'tinytable_css_tcwm5wikiwj8rvg4mux7'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 4], [1, 4], [2, 4], [3, 4], [4, 4], [5, 4], [6, 4], [7, 4]], class: 'tinytable_css_txg0lqhgbpcic9ibyax2'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 5], [1, 5], [2, 5], [3, 5], [4, 5], [5, 5], [6, 5], [7, 5]], class: 'tinytable_css_rmefdfyhbuhkldvl77un'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 0], [0, 1], [0, 2], [0, 3], [0, 4], [0, 5]], class: 'tinytable_css_m9ldekch29sil5swdde8'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 1], [0, 2]], class: 'tinytable_css_ef8g4omg4ycqo1e9kqdi'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 0], [1, 0], [2, 0], [3, 0], [4, 0], [5, 0], [6, 0], [7, 0], [0, 1], [1, 1], [2, 1], [3, 1], [4, 1], [5, 1], [6, 1], [7, 1], [0, 2], [1, 2], [2, 2], [3, 2], [4, 2], [5, 2], [6, 2], [7, 2], [0, 3], [1, 3], [2, 3], [3, 3], [4, 3], [5, 3], [6, 3], [7, 3]], class: 'tinytable_css_dlf10f7lx1bp3da6omu0'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });
    </script>

    <style>
.table td.tinytable_css_exn405498uollp1zsjup, .table th.tinytable_css_exn405498uollp1zsjup {    border-bottom: solid 0.1em #d3d8dc; }
.table td.tinytable_css_t4if5s3u0ps5egb44p4w, .table th.tinytable_css_t4if5s3u0ps5egb44p4w {  width: 38.0952380952381%;  }
.table td.tinytable_css_i9uqsk3gdql67vqwuke2, .table th.tinytable_css_i9uqsk3gdql67vqwuke2 {  width: 14.2857142857143%;  }
.table td.tinytable_css_vqmduapq1n2nfbxno0e1, .table th.tinytable_css_vqmduapq1n2nfbxno0e1 {  width: 14.2857142857143%;  }
.table td.tinytable_css_tcwm5wikiwj8rvg4mux7, .table th.tinytable_css_tcwm5wikiwj8rvg4mux7 {  width: 14.2857142857143%;  }
.table td.tinytable_css_txg0lqhgbpcic9ibyax2, .table th.tinytable_css_txg0lqhgbpcic9ibyax2 {  width: 9.52380952380953%;  }
.table td.tinytable_css_rmefdfyhbuhkldvl77un, .table th.tinytable_css_rmefdfyhbuhkldvl77un {  width: 9.52380952380953%;  }
.table td.tinytable_css_m9ldekch29sil5swdde8, .table th.tinytable_css_m9ldekch29sil5swdde8 {    text-align: center; }
.table td.tinytable_css_ef8g4omg4ycqo1e9kqdi, .table th.tinytable_css_ef8g4omg4ycqo1e9kqdi {    border-bottom: solid 0.05em #d3d8dc; }
.table td.tinytable_css_dlf10f7lx1bp3da6omu0, .table th.tinytable_css_dlf10f7lx1bp3da6omu0 {    text-align: left; }
    </style>
    <div class="container">
      <table class="table table-borderless" id="tinytable_2i23uijusjbj7igeqj4w" style="table-layout: fixed; width: 100% !important; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        <thead>
<tr>
<th scope="col" align="center" colspan=-Inf></th>
<th scope="col" align="center" colspan=3>Estimates</th>
<th scope="col" align="center" colspan=2>Test for Equality</th>
</tr>
        <caption>Example table</caption>
              <tr>
                <th scope="col">Characteristic</th>
                <th scope="col">Wave 1
(2018-19)
N=1003</th>
                <th scope="col">Wave 2 (2019-20) N=1110</th>
                <th scope="col">Wave 4 (2021-22) N=1028</th>
                <th scope="col">Statistic<sup>a</sup></th>
                <th scope="col">p-value</th>
              </tr>
        </thead>
        <tfoot><tr><td colspan='6'><sup>a</sup> Blah.</td></tr></tfoot>
        <tbody>
                <tr>
                  <td>Female, n (%)</td>
                  <td>580 (57.8)</td>
                  <td>653 (58.8)</td>
                  <td>612 (59.5)</td>
                  <td>0.616</td>
                  <td>0.735</td>
                </tr>
                <tr>
                  <td>Current smoker, n (%)</td>
                  <td>257 (25.6)</td>
                  <td>295 (26.6)</td>
                  <td>265 (25.8)</td>
                  <td>0.292</td>
                  <td>0.864</td>
                </tr>
                <tr>
                  <td>Any smoke exposure, n (%)</td>
                  <td>795 (79.3)</td>
                  <td>898 (80.9)</td>
                  <td>857 (83.4)</td>
                  <td>5.616</td>
                  <td>0.060</td>
                </tr>
                <tr>
                  <td>Age in years, Mean (SD)</td>
                  <td>26.1 (3.7)</td>
                  <td>25.7 (3.5)</td>
                  <td>26.1 (4.0)</td>
                  <td>3.273</td>
                  <td>0.076</td>
                </tr>
                <tr>
                  <td>BMI (kg/m2), Mean (SD)</td>
                  <td>60.1 (9.3)</td>
                  <td>61.1 (9.1)</td>
                  <td>63.3 (9.0)</td>
                  <td>31.980</td>
                  <td>0.000</td>
                </tr>
                <tr>
                  <td>Waist circumference (cm), Mean (SD)</td>
                  <td>86.8 (10.2)</td>
                  <td>87.4 (9.4)</td>
                  <td>91.4 (10.7)</td>
                  <td>54.035</td>
                  <td>0.000</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```


:::
:::


Now what about this idea of a margin note?

::: column-margin
We know from *the first fundamental theorem of calculus* that blah blah blah.
:::

How does that differ from more generic comment using the `changes` package? Let's see 
\comment{Maybe I shouldn't have written this?}
How does that work?

Let's try straight HTML code, referencing @tbl-table1

::: {#tbl-table1 .cell tbl-cap="Table 2 caption."}
::: {.cell-output-display}




```{=html}
<!-- preamble start -->

    <script>
      function styleCell_tinytable_06xukigh80c1jrp5q689(tableId, cellCoordinates, cssClass) {
            var table = document.getElementById("tinytable_2i23uijusjbj7igeqj4w");
            cellCoordinates.forEach(([i, j]) => {
                table.rows[i].cells[j].classList.add(cssClass);
            });
        }

      function insertSpanRow(i, colspan, content) {
        var table = document.getElementById('tinytable_2i23uijusjbj7igeqj4w');
        var newRow = table.insertRow(i);
        var newCell = newRow.insertCell(0);
        newCell.setAttribute("colspan", colspan);
        // newCell.innerText = content;
        // this may be unsafe, but innerText does not interpret <br>
        newCell.innerHTML = content;
      }
      function spanCell_tinytable_06xukigh80c1jrp5q689(i, j, rowspan, colspan) {
        var table = document.getElementById("tinytable_2i23uijusjbj7igeqj4w");
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

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[1, 0], [1, 1], [1, 2], [1, 3], [1, 4], [1, 5]], class: 'tinytable_css_exn405498uollp1zsjup'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 0], [1, 0], [2, 0], [3, 0], [4, 0], [5, 0], [6, 0], [7, 0]], class: 'tinytable_css_t4if5s3u0ps5egb44p4w'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 1], [1, 1], [2, 1], [3, 1], [4, 1], [5, 1], [6, 1], [7, 1]], class: 'tinytable_css_i9uqsk3gdql67vqwuke2'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 2], [1, 2], [2, 2], [3, 2], [4, 2], [5, 2], [6, 2], [7, 2]], class: 'tinytable_css_vqmduapq1n2nfbxno0e1'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 3], [1, 3], [2, 3], [3, 3], [4, 3], [5, 3], [6, 3], [7, 3]], class: 'tinytable_css_tcwm5wikiwj8rvg4mux7'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 4], [1, 4], [2, 4], [3, 4], [4, 4], [5, 4], [6, 4], [7, 4]], class: 'tinytable_css_txg0lqhgbpcic9ibyax2'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 5], [1, 5], [2, 5], [3, 5], [4, 5], [5, 5], [6, 5], [7, 5]], class: 'tinytable_css_rmefdfyhbuhkldvl77un'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 0], [0, 1], [0, 2], [0, 3], [0, 4], [0, 5]], class: 'tinytable_css_m9ldekch29sil5swdde8'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 1], [0, 2]], class: 'tinytable_css_ef8g4omg4ycqo1e9kqdi'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });

     window.addEventListener('load', function () {
         const cellStyles = [
             {coords: [[0, 0], [1, 0], [2, 0], [3, 0], [4, 0], [5, 0], [6, 0], [7, 0], [0, 1], [1, 1], [2, 1], [3, 1], [4, 1], [5, 1], [6, 1], [7, 1], [0, 2], [1, 2], [2, 2], [3, 2], [4, 2], [5, 2], [6, 2], [7, 2], [0, 3], [1, 3], [2, 3], [3, 3], [4, 3], [5, 3], [6, 3], [7, 3]], class: 'tinytable_css_dlf10f7lx1bp3da6omu0'},
         ];
         cellStyles.forEach(({coords, class: cssClass}) => {
             styleCell_tinytable_06xukigh80c1jrp5q689('tinytable_szxl8eb7ubljmabuxmyx', coords, cssClass);
         });
     });
    </script>

    <style>
.table td.tinytable_css_exn405498uollp1zsjup, .table th.tinytable_css_exn405498uollp1zsjup {    border-bottom: solid 0.1em #d3d8dc; }
.table td.tinytable_css_t4if5s3u0ps5egb44p4w, .table th.tinytable_css_t4if5s3u0ps5egb44p4w {  width: 38.0952380952381%;  }
.table td.tinytable_css_i9uqsk3gdql67vqwuke2, .table th.tinytable_css_i9uqsk3gdql67vqwuke2 {  width: 14.2857142857143%;  }
.table td.tinytable_css_vqmduapq1n2nfbxno0e1, .table th.tinytable_css_vqmduapq1n2nfbxno0e1 {  width: 14.2857142857143%;  }
.table td.tinytable_css_tcwm5wikiwj8rvg4mux7, .table th.tinytable_css_tcwm5wikiwj8rvg4mux7 {  width: 14.2857142857143%;  }
.table td.tinytable_css_txg0lqhgbpcic9ibyax2, .table th.tinytable_css_txg0lqhgbpcic9ibyax2 {  width: 9.52380952380953%;  }
.table td.tinytable_css_rmefdfyhbuhkldvl77un, .table th.tinytable_css_rmefdfyhbuhkldvl77un {  width: 9.52380952380953%;  }
.table td.tinytable_css_m9ldekch29sil5swdde8, .table th.tinytable_css_m9ldekch29sil5swdde8 {    text-align: center; }
.table td.tinytable_css_ef8g4omg4ycqo1e9kqdi, .table th.tinytable_css_ef8g4omg4ycqo1e9kqdi {    border-bottom: solid 0.05em #d3d8dc; }
.table td.tinytable_css_dlf10f7lx1bp3da6omu0, .table th.tinytable_css_dlf10f7lx1bp3da6omu0 {    text-align: left; }
    </style>
    <div class="container">
      <table class="table table-borderless" id="tinytable_2i23uijusjbj7igeqj4w" style="table-layout: fixed; width: 100% !important; margin-left: auto; margin-right: auto;" data-quarto-disable-processing='true'>
        <thead>
<tr>
<th scope="col" align="center" colspan=-Inf></th>
<th scope="col" align="center" colspan=3>Estimates</th>
<th scope="col" align="center" colspan=2>Test for Equality</th>
</tr>
        <caption>Example table</caption>
              <tr>
                <th scope="col">Characteristic</th>
                <th scope="col">Wave 1
(2018-19)
N=1003</th>
                <th scope="col">Wave 2 (2019-20) N=1110</th>
                <th scope="col">Wave 4 (2021-22) N=1028</th>
                <th scope="col">Statistic<sup>a</sup></th>
                <th scope="col">p-value</th>
              </tr>
        </thead>
        <tfoot><tr><td colspan='6'><sup>a</sup> Blah.</td></tr></tfoot>
        <tbody>
                <tr>
                  <td>Female, n (%)</td>
                  <td>580 (57.8)</td>
                  <td>653 (58.8)</td>
                  <td>612 (59.5)</td>
                  <td>0.616</td>
                  <td>0.735</td>
                </tr>
                <tr>
                  <td>Current smoker, n (%)</td>
                  <td>257 (25.6)</td>
                  <td>295 (26.6)</td>
                  <td>265 (25.8)</td>
                  <td>0.292</td>
                  <td>0.864</td>
                </tr>
                <tr>
                  <td>Any smoke exposure, n (%)</td>
                  <td>795 (79.3)</td>
                  <td>898 (80.9)</td>
                  <td>857 (83.4)</td>
                  <td>5.616</td>
                  <td>0.060</td>
                </tr>
                <tr>
                  <td>Age in years, Mean (SD)</td>
                  <td>26.1 (3.7)</td>
                  <td>25.7 (3.5)</td>
                  <td>26.1 (4.0)</td>
                  <td>3.273</td>
                  <td>0.076</td>
                </tr>
                <tr>
                  <td>BMI (kg/m2), Mean (SD)</td>
                  <td>60.1 (9.3)</td>
                  <td>61.1 (9.1)</td>
                  <td>63.3 (9.0)</td>
                  <td>31.980</td>
                  <td>0.000</td>
                </tr>
                <tr>
                  <td>Waist circumference (cm), Mean (SD)</td>
                  <td>86.8 (10.2)</td>
                  <td>87.4 (9.4)</td>
                  <td>91.4 (10.7)</td>
                  <td>54.035</td>
                  <td>0.000</td>
                </tr>
        </tbody>
      </table>
    </div>
<!-- hack to avoid NA insertion in last line -->
```




:::
:::