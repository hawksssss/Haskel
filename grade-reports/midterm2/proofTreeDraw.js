/*****************************
 * prooftreeDraw.js
 * v1.1: Added support for serializing/deserializing
 * v1.0: Initial
 *
 * Written by Terence Nip
 ****************************/


/***************************
 * loadOperator
 * Inserts the appropriate operator into operator spans.
 ***************************/
  function loadOperator() {
    $('.operator').each(function() {
      $(this).html('$\\Downarrow$');
    });
  }

/***************************
 * giveWidths
 * Assigns widths to each node in our proof tree.
 ***************************/
  function giveWidths(elems, hierarchy, currLevel) {
    var relevantElems = [];

    // An element is relevant if they have the same level ID.
    for (var i = 0; i < elems.length; i++) {
      if (elems[i].classList.contains('proofTreeNest')) {
        relevantElems.push(elems[i]);
      }
    }

    // elemCount is used for giving each "node" in our tree a unique ID.
    var elemCount = 0;
    var width = $('#proofTreeContainer').width();

    // For every level, style the elements accordingly.
    for (var i = 0; i < relevantElems.length; i++) {
      var currChar = String.fromCharCode(97 + elemCount);
      elems[i].id = hierarchy + '-l' + currLevel + currChar;

      if (elems[i].id == 'root-l0a') {
        width = $('#proofTreeContainer').width();
      } else {
        width = (Math.floor($(elems[0]).parent('.proofTreeNest').width() - 25) /
          relevantElems.length);
      }

      // Give the checkbox this ID!
      $(elems[i]).children('.proofTreeLabel')
        .children('input[type=checkbox]')
        .attr('data-checkedoptional', 'submittedAnswer.' + elems[i].id);

      var elem = $('#' + elems[i].id);
      elem.outerWidth(width + 'px');
      elem.css('display', 'table-cell');
      elem.css('vertical-align', 'bottom');
      elem.css('padding-left', '3px');
      elem.css('padding-right', '3px');
      if (elems[i].classList.contains('proofTreeNest')) {
        elem = elem.children('.proofTreeNest:last-child');
        elem.css('margin', '0');
        elem.css('text-align', 'center');
        elem.outerWidth(width + 'px');
        elem.css('display', 'table-cell');
        elem.css('vertical-align', 'bottom');
      }

      elemCount++;

      if ($(elems[i]).children('.proofTreeNest').length > 0) {
        giveWidths($(elems[i]).children('.proofTreeNest'), elems[i].id, currLevel + 1);
      }
    }

    // Every time we resize, we need to reserialize.
    serializeTree();
  }

/***************************
 * serializeTree
 * Serializes the tree for submission.
 ***************************/
  function serializeTree() {
    var result = {};

    $('#proofTreeContainer .proofTreeNest').each(function() {
      var treeLocation = $(this).attr('id');
      var lineLeft = $('#' + treeLocation +  ' > .proofTreeLine > .proofTreeLineLeft').text();
      var lineRight = $('#' + treeLocation + ' > .proofTreeLine >   .proofTreeLineRight').text();
      var lineCondition = $('#' + treeLocation + ' .proofTreeLine  > .proofTreeLineCondition').text();
      var lineLabel = $('#' + treeLocation + ' > .proofTreeLabel >  .proofTreeDropdownLabel').val();
      var lineSideCondition = $('#' + treeLocation + ' > .proofTreeSideCondition').val();

      result[treeLocation] = {};
      result[treeLocation]['left'] = $.trim(lineLeft);
      result[treeLocation]['middle'] = $.trim(lineRight);
      result[treeLocation]['right'] = $.trim(lineCondition);
      result[treeLocation]['label'] = $.trim(lineLabel);
      result[treeLocation]['sideCondition'] = $.trim(lineSideCondition);
    });

    var keypress = jQuery.Event('input');
    $('#serializedTree').val(JSON.stringify(result));
    $('#serializedTree').click().trigger(keypress).blur();

    return result;
  }

  function unmarshal() {
      window.unmarshalled = true;
      var rawString = window.ans;
      var jsonTree = JSON.parse(rawString);
      var treePositions = Object.keys(jsonTree).sort();

      for (var i = 0; i < treePositions.length; i++) {
        var currentPosition = treePositions[i];
        var parentPosition = treePositions[i].split('-');
        parentPosition.pop();
        parentPosition = parentPosition.join('-');

        var currentPositionId = '#' + currentPosition;
        var parentPositionId = '#' + parentPosition;

        var labelVal = jsonTree[currentPosition]['label'];
        var leftVal = jsonTree[currentPosition]['left'];
        var middleVal = jsonTree[currentPosition]['middle'];
        var rightVal = jsonTree[currentPosition]['right'];
        var sideConditionVal = jsonTree[currentPosition]['sideCondition'];

        // We handle root-l0a separately from everything else.
        // More specifically, we only update the dropdown.
          // We need to get the current level in the tree - and as such, we need that
          // of the parent.
          var currLevel = 1;
          var currLevelText = "l" + currLevel;

          var conditionClause = (sideConditionVal == '') ? "where " : "";
          conditionClause = '';
          var style = '';
          var operator = ' <span class="operator">$\\downarrow$</span> ';

          var nestHtml = '<div class="proofTreeNest ' + currLevelText + '" id="' + currentPosition + '">' +
            '<div class="proofTreeAddSubproof">' +
              '<span class="proofTreeSideConditionLink">' +
              '</span>' +
            '</div>' +
            '<div class="proofTreeActions">' +
            '</div>' +
            '<input type="text" class="proofTreeSideCondition" placeholder="Side Condition" />' +
            '<div class="proofTreeLabel">' +
              labelVal +
            '</div>' +
            '<div class="proofTreeLine" ' + style + '>' +
              '<span class="proofTreeLineLeft tt">' +
                leftVal +
              '</span>' +
                operator +
              '<span class="proofTreeLineRight tt">' +
                middleVal +
              '</span>' +
              '<span class="proofTreeLineConditionPhrase">' +
                conditionClause +
              '</span>' +
              '<span class="proofTreeLineCondition tt">' +
                rightVal +
              '</span>' +
            '</div>' +
         '</div>';

          // Add the new subtree into the proof tree
          if($(parentPositionId).children('.proofTreeNest').length == 0) {
            $(parentPositionId).prepend(nestHtml);
          } else {
            $(parentPositionId + ' > .proofTreeNest').last().after(nestHtml);
          }

          // Do the resizing and add the corresponding operator, rerun MathJax
          giveWidths($('#proofTreeContainer').children(), 'root', 0);
          loadOperator();
          MathJax.Hub.Queue(["Typeset",MathJax.Hub]);

          //$(currentPositionId + ' > .proofTreeLabel').text(labelVal);
          $(currentPositionId + ' > .proofTreeSideCondition').val(sideConditionVal);
          if (sideConditionVal != '') {
            $(currentPositionId + ' > .proofTreeSideCondition').css('display', 'inline');
          }
      }
        $('#root-l0a').append('<div class="proofTreeLine"></div>');
        $('#root-l0a > .proofTreeLine').prepend(
            '<div class="proofTreeLabel">' +
              jsonTree['root-l0a']['label'] +
            '</div>' +
            '<span class="proofTreeLineLeft tt">' +
              jsonTree['root-l0a']['left'] +
            '</span>' +
              operator +
            '<span class="proofTreeLineRight tt">' +
              jsonTree['root-l0a']['middle'] +
            '</span>' +
            '<span class="proofTreeLineConditionPhrase">' +
              conditionClause +
            '</span>' +
            '<span class="proofTreeLineCondition tt">' +
              jsonTree['root-l0a']['right'] +
            '</span>'
         );

        giveWidths($('#proofTreeContainer').children(), 'root', 0);
  }
