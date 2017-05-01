(function(A){A.widget("ui.draggable",{init:function(){var B=this.options;this.element.mouse({executor:this,delay:B.delay,distance:B.distance,dragPrevention:B.cancel,start:this.start,stop:this.stop,drag:this.drag,condition:function(D){var C=!this.options.handle||!A(this.options.handle,this.element).length?true:false;if(!C){A(this.options.handle,this.element).each(function(){if(this==D.target){C=true}})}return !(D.target.className.indexOf("ui-resizable-handle")!=-1||this.options.disabled)&&C}});if(B.helper=="original"&&!(/(relative|absolute|fixed)/).test(this.element.css("position"))){this.element.css("position","relative")}},start:function(E){var G=this.options;if(A.ui.ddmanager){A.ui.ddmanager.current=this}this.helper=A.isFunction(G.helper)?A(G.helper.apply(this.element[0],[E])):(G.helper=="clone"?this.element.clone():this.element);if(!this.helper.parents("body").length){this.helper.appendTo((G.appendTo=="parent"?this.element[0].parentNode:G.appendTo))}if(!this.helper.css("position")||this.helper.css("position")=="static"){this.helper.css("position","absolute")}this.margins={left:(parseInt(this.element.css("marginLeft"),10)||0),top:(parseInt(this.element.css("marginTop"),10)||0)};this.cssPosition=this.helper.css("position");this.offset=this.element.offset();this.offset={top:this.offset.top-this.margins.top,left:this.offset.left-this.margins.left};this.offset.click={left:E.pageX-this.offset.left,top:E.pageY-this.offset.top};this.offsetParent=this.helper.offsetParent();var B=this.offsetParent.offset();this.offset.parent={top:B.top+(parseInt(this.offsetParent.css("borderTopWidth"),10)||0),left:B.left+(parseInt(this.offsetParent.css("borderLeftWidth"),10)||0)};var D=this.element.position();this.offset.relative=this.cssPosition=="relative"?{top:D.top-(parseInt(this.helper.css("top"),10)||0)+this.offsetParent[0].scrollTop,left:D.left-(parseInt(this.helper.css("left"),10)||0)+this.offsetParent[0].scrollLeft}:{top:0,left:0};this.originalPosition=this.generatePosition(E);this.helperProportions={width:this.helper.outerWidth(),height:this.helper.outerHeight()};if(G.cursorAt){if(G.cursorAt.left!=undefined){this.offset.click.left=G.cursorAt.left}if(G.cursorAt.right!=undefined){this.offset.click.left=this.helperProportions.width-G.cursorAt.right}if(G.cursorAt.top!=undefined){this.offset.click.top=G.cursorAt.top}if(G.cursorAt.bottom!=undefined){this.offset.click.top=this.helperProportions.height-G.cursorAt.bottom}}if(G.containment){if(G.containment=="parent"){G.containment=this.helper[0].parentNode}if(G.containment=="document"){this.containment=[0,0,A(document).width(),(A(document).height()||document.body.parentNode.scrollHeight)]}if(!(/^(document|window|parent)$/).test(G.containment)){var C=A(G.containment)[0];var F=A(G.containment).offset();this.containment=[F.left+(parseInt(A(C).css("borderLeftWidth"),10)||0)-this.offset.relative.left-this.offset.parent.left,F.top+(parseInt(A(C).css("borderTopWidth"),10)||0)-this.offset.relative.top-this.offset.parent.top,F.left+Math.max(C.scrollWidth,C.offsetWidth)-(parseInt(A(C).css("borderLeftWidth"),10)||0)-this.offset.relative.left-this.offset.parent.left-this.helperProportions.width-this.margins.left-(parseInt(this.element.css("marginRight"),10)||0),F.top+Math.max(C.scrollHeight,C.offsetHeight)-(parseInt(A(C).css("borderTopWidth"),10)||0)-this.offset.relative.top-this.offset.parent.top-this.helperProportions.height-this.margins.top-(parseInt(this.element.css("marginBottom"),10)||0)]}}this.propagate("start",E);this.helperProportions={width:this.helper.outerWidth(),height:this.helper.outerHeight()};if(A.ui.ddmanager&&!G.dropBehaviour){A.ui.ddmanager.prepareOffsets(this,E)}return false},convertPositionTo:function(C,D){if(!D){D=this.position}var B=C=="absolute"?1:-1;return{top:(D.top+this.offset.relative.top*B+this.offset.parent.top*B-(this.cssPosition=="fixed"?0:this.offsetParent[0].scrollTop)*B+this.margins.top*B),left:(D.left+this.offset.relative.left*B+this.offset.parent.left*B-(this.cssPosition=="fixed"?0:this.offsetParent[0].scrollLeft)*B+this.margins.left*B)}},generatePosition:function(E){var F=this.options;var B={top:(E.pageY-this.offset.click.top-this.offset.relative.top-this.offset.parent.top+(this.cssPosition=="fixed"?0:this.offsetParent[0].scrollTop)),left:(E.pageX-this.offset.click.left-this.offset.relative.left-this.offset.parent.left+(this.cssPosition=="fixed"?0:this.offsetParent[0].scrollLeft))};if(!this.originalPosition){return B}if(this.containment){if(B.left<this.containment[0]){B.left=this.containment[0]}if(B.top<this.containment[1]){B.top=this.containment[1]}if(B.left>this.containment[2]){B.left=this.containment[2]}if(B.top>this.containment[3]){B.top=this.containment[3]}}if(F.grid){var D=this.originalPosition.top+Math.round((B.top-this.originalPosition.top)/F.grid[1])*F.grid[1];B.top=this.containment?(!(D<this.containment[1]||D>this.containment[3])?D:(!(D<this.containment[1])?D-F.grid[1]:D+F.grid[1])):D;var C=this.originalPosition.left+Math.round((B.left-this.originalPosition.left)/F.grid[0])*F.grid[0];B.left=this.containment?(!(C<this.containment[0]||C>this.containment[2])?C:(!(C<this.containment[0])?C-F.grid[0]:C+F.grid[0])):C}return B},drag:function(B){this.position=this.generatePosition(B);this.positionAbs=this.convertPositionTo("absolute");this.position=this.propagate("drag",B)||this.position;if(!this.options.axis||this.options.axis=="x"){this.helper[0].style.left=this.position.left+"px"}if(!this.options.axis||this.options.axis=="y"){this.helper[0].style.top=this.position.top+"px"}if(A.ui.ddmanager){A.ui.ddmanager.drag(this,B)}return false},stop:function(C){if(A.ui.ddmanager&&!this.options.dropBehaviour){A.ui.ddmanager.drop(this,C)}if(this.options.revert){var B=this;A(this.helper).animate(this.originalPosition,parseInt(this.options.revert,10)||500,function(){B.propagate("stop",C);B.clear()})}else{this.propagate("stop",C);this.clear()}return false},clear:function(){if(this.options.helper!="original"&&!this.cancelHelperRemoval){this.helper.remove()}if(A.ui.ddmanager){A.ui.ddmanager.current=null}this.helper=null;this.cancelHelperRemoval=false},plugins:{},ui:function(B){return{helper:this.helper,position:this.position,absolutePosition:this.positionAbs,options:this.options}},propagate:function(C,B){A.ui.plugin.call(this,C,[B,this.ui()]);return this.element.triggerHandler(C=="drag"?C:"drag"+C,[B,this.ui()],this.options[C])},destroy:function(){if(!this.element.data("draggable")){return }this.element.removeData("draggable").unbind(".draggable").mouse("destroy")},enable:function(){this.options.disabled=false},disable:function(){this.options.disabled=true}});A.ui.draggable.defaults={helper:"original",appendTo:"parent",cancel:["input","textarea","button","select","option"],distance:1,delay:0};A.ui.plugin.add("draggable","cursor",{start:function(D,C){var B=A("body");if(B.css("cursor")){C.options._cursor=B.css("cursor")}B.css("cursor",C.options.cursor)},stop:function(C,B){if(B.options._cursor){A("body").css("cursor",B.options._cursor)}}});A.ui.plugin.add("draggable","zIndex",{start:function(D,C){var B=A(C.helper);if(B.css("zIndex")){C.options._zIndex=B.css("zIndex")}B.css("zIndex",C.options.zIndex)},stop:function(C,B){if(B.options._zIndex){A(B.helper).css("zIndex",B.options._zIndex)}}});A.ui.plugin.add("draggable","opacity",{start:function(D,C){var B=A(C.helper);if(B.css("opacity")){C.options._opacity=B.css("opacity")}B.css("opacity",C.options.opacity)},stop:function(C,B){if(B.options._opacity){A(B.helper).css("opacity",B.options._opacity)}}});A.ui.plugin.add("draggable","iframeFix",{start:function(C,B){A(B.options.iframeFix===true?"iframe":B.options.iframeFix).each(function(){A('<div class="ui-draggable-iframeFix" style="background: #fff;"></div>').css({width:this.offsetWidth+"px",height:this.offsetHeight+"px",position:"absolute",opacity:"0.001",zIndex:1000}).css(A(this).offset()).appendTo("body")})},stop:function(C,B){A("div.DragDropIframeFix").each(function(){this.parentNode.removeChild(this)})}});A.ui.plugin.add("draggable","scroll",{start:function(D,C){var E=C.options;var B=A(this).data("draggable");E.scrollSensitivity=E.scrollSensitivity||20;E.scrollSpeed=E.scrollSpeed||20;B.overflowY=function(F){do{if(/auto|scroll/.test(F.css("overflow"))||(/auto|scroll/).test(F.css("overflow-y"))){return F}F=F.parent()}while(F[0].parentNode);return A(document)}(this);B.overflowX=function(F){do{if(/auto|scroll/.test(F.css("overflow"))||(/auto|scroll/).test(F.css("overflow-x"))){return F}F=F.parent()}while(F[0].parentNode);return A(document)}(this);if(B.overflowY[0]!=document&&B.overflowY[0].tagName!="HTML"){B.overflowYOffset=B.overflowY.offset()}if(B.overflowX[0]!=document&&B.overflowX[0].tagName!="HTML"){B.overflowXOffset=B.overflowX.offset()}},drag:function(D,C){var E=C.options;var B=A(this).data("draggable");if(B.overflowY[0]!=document&&B.overflowY[0].tagName!="HTML"){if((B.overflowYOffset.top+B.overflowY[0].offsetHeight)-D.pageY<E.scrollSensitivity){B.overflowY[0].scrollTop=B.overflowY[0].scrollTop+E.scrollSpeed}if(D.pageY-B.overflowYOffset.top<E.scrollSensitivity){B.overflowY[0].scrollTop=B.overflowY[0].scrollTop-E.scrollSpeed}}else{if(D.pageY-A(document).scrollTop()<E.scrollSensitivity){A(document).scrollTop(A(document).scrollTop()-E.scrollSpeed)}if(A(window).height()-(D.pageY-A(document).scrollTop())<E.scrollSensitivity){A(document).scrollTop(A(document).scrollTop()+E.scrollSpeed)}}if(B.overflowX[0]!=document&&B.overflowX[0].tagName!="HTML"){if((B.overflowXOffset.left+B.overflowX[0].offsetWidth)-D.pageX<E.scrollSensitivity){B.overflowX[0].scrollLeft=B.overflowX[0].scrollLeft+E.scrollSpeed}if(D.pageX-B.overflowXOffset.left<E.scrollSensitivity){B.overflowX[0].scrollLeft=B.overflowX[0].scrollLeft-E.scrollSpeed}}else{if(D.pageX-A(document).scrollLeft()<E.scrollSensitivity){A(document).scrollLeft(A(document).scrollLeft()-E.scrollSpeed)}if(A(window).width()-(D.pageX-A(document).scrollLeft())<E.scrollSensitivity){A(document).scrollLeft(A(document).scrollLeft()+E.scrollSpeed)}}}});A.ui.plugin.add("draggable","snap",{start:function(D,C){var B=A(this).data("draggable");B.snapElements=[];A(C.options.snap===true?".ui-draggable":C.options.snap).each(function(){var F=A(this);var E=F.offset();if(this!=B.element[0]){B.snapElements.push({item:this,width:F.outerWidth(),height:F.outerHeight(),top:E.top,left:E.left})}})},drag:function(J,N){var I=A(this).data("draggable");var L=N.options.snapTolerance||20;var D=N.absolutePosition.left,C=D+I.helperProportions.width,P=N.absolutePosition.top,O=P+I.helperProportions.height;for(var H=I.snapElements.length-1;H>=0;H--){var E=I.snapElements[H].left,B=E+I.snapElements[H].width,R=I.snapElements[H].top,M=R+I.snapElements[H].height;if(!((E-L<D&&D<B+L&&R-L<P&&P<M+L)||(E-L<D&&D<B+L&&R-L<O&&O<M+L)||(E-L<C&&C<B+L&&R-L<P&&P<M+L)||(E-L<C&&C<B+L&&R-L<O&&O<M+L))){continue}if(N.options.snapMode!="inner"){var K=Math.abs(R-O)<=20;var Q=Math.abs(M-P)<=20;var G=Math.abs(E-C)<=20;var F=Math.abs(B-D)<=20;if(K){N.position.top=I.convertPositionTo("relative",{top:R-I.helperProportions.height,left:0}).top}if(Q){N.position.top=I.convertPositionTo("relative",{top:M,left:0}).top}if(G){N.position.left=I.convertPositionTo("relative",{top:0,left:E-I.helperProportions.width}).left}if(F){N.position.left=I.convertPositionTo("relative",{top:0,left:B}).left}}if(N.options.snapMode!="outer"){var K=Math.abs(R-P)<=20;var Q=Math.abs(M-O)<=20;var G=Math.abs(E-D)<=20;var F=Math.abs(B-C)<=20;if(K){N.position.top=I.convertPositionTo("relative",{top:R,left:0}).top}if(Q){N.position.top=I.convertPositionTo("relative",{top:M-I.helperProportions.height,left:0}).top}if(G){N.position.left=I.convertPositionTo("relative",{top:0,left:E}).left}if(F){N.position.left=I.convertPositionTo("relative",{top:0,left:B-I.helperProportions.width}).left}}}}});A.ui.plugin.add("draggable","connectToSortable",{start:function(D,C){var B=A(this).data("draggable");B.sortable=A.data(A(C.options.connectToSortable)[0],"sortable");B.sortableOffset=B.sortable.element.offset();B.sortableOuterWidth=B.sortable.element.outerWidth();B.sortableOuterHeight=B.sortable.element.outerHeight();if(B.sortable.options.revert){B.sortable.shouldRevert=true}},stop:function(D,C){var E=A(this).data("draggable");var B=E.sortable;if(B.isOver){B.isOver=0;E.cancelHelperRemoval=true;B.cancelHelperRemoval=false;if(B.shouldRevert){B.options.revert=true}B.stop(D);B.options.helper="original"}},drag:function(F,E){var G=A(this).data("draggable");var D=G.sortable;G.position.absolute=E.absolutePosition;if(D.intersectsWith.call(G,{left:G.sortableOffset.left,top:G.sortableOffset.top,width:G.sortableOuterWidth,height:G.sortableOuterHeight})){if(!D.isOver){D.isOver=1;var B=D.options.placeholderElement?A(D.options.placeholderElement,A(D.options.items,D.element)).innerHeight():A(D.options.items,D.element).innerHeight();var C=D.options.placeholderElement?A(D.options.placeholderElement,A(D.options.items,D.element)).innerWidth():A(D.options.items,D.element).innerWidth();D.currentItem=A(this).clone().appendTo(D.element);D.options.helper=function(){return E.helper[0]};D.start(F);D.clickOffset.top=G.offset.click.top;D.clickOffset.left=G.offset.click.left;D.offset.left-=E.absolutePosition.left-D.position.absolute.left;D.offset.top-=E.absolutePosition.top-D.position.absolute.top;D.helperProportions={width:C,height:B};E.helper.animate({height:B,width:C},500);G.propagate("toSortable",F)}if(D.currentItem){D.drag(F)}}else{if(D.isOver){D.isOver=0;D.cancelHelperRemoval=true;D.options.revert=false;D.stop(F);D.options.helper="original";D.currentItem.remove();D.placeholder.remove();E.helper.animate({height:this.innerHeight(),width:this.innerWidth()},500);G.propagate("fromSortable",F)}}}});A.ui.plugin.add("draggable","stack",{start:function(D,B){var C=A.makeArray(A(B.options.stack.group)).sort(function(F,E){return(parseInt(A(F).css("zIndex"))||B.options.stack.min)-(parseInt(A(E).css("zIndex"))||B.options.stack.min)});A(C).each(function(E){this.style.zIndex=B.options.stack.min+E});this[0].style.zIndex=B.options.stack.min+C.length}})})(jQuery);