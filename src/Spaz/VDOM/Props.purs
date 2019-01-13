module Spaz.VDOM.Props where
import Prelude
import React.DOM.Props as P
import React.SyntheticEvent as Event
import Spaz (Eff, Interpret)

type Props st act = Interpret st act -> P.Props

aria :: ∀ ariaAttrs st act. { | ariaAttrs } -> Props st act
aria v _ = P.aria v

_data :: ∀ dataAttrs st act. { | dataAttrs } -> Props st act
_data v _ = P._data v

style :: ∀ style st act. { | style } -> Props st act
style v _ = P.style v

dangerouslySetInnerHTML :: ∀ st act. { __html :: String } -> Props st act
dangerouslySetInnerHTML v _ = P.dangerouslySetInnerHTML v

accept :: ∀ st act. String -> Props st act
accept v _ = P.accept v

acceptCharset :: ∀ st act. String -> Props st act
acceptCharset v _ = P.acceptCharset v

accessKey :: ∀ st act. String -> Props st act
accessKey v _ = P.accessKey v

action :: ∀ st act. String -> Props st act
action v _ = P.action v

allowFullScreen :: ∀ st act. Boolean -> Props st act
allowFullScreen v _ = P.allowFullScreen v

allowTransparency :: ∀ st act. Boolean -> Props st act
allowTransparency v _ = P.allowTransparency v

alt :: ∀ st act. String -> Props st act
alt v _ = P.alt v

async :: ∀ st act. Boolean -> Props st act
async v _ = P.async v

autoComplete :: ∀ st act. String -> Props st act
autoComplete v _ = P.autoComplete v

autoFocus :: ∀ st act. Boolean -> Props st act
autoFocus v _ = P.autoFocus v

autoPlay :: ∀ st act. Boolean -> Props st act
autoPlay v _ = P.autoPlay v

capture :: ∀ st act. Boolean -> Props st act
capture v _ = P.capture v

cellPadding :: ∀ st act. String -> Props st act
cellPadding v _ = P.cellPadding v

cellSpacing :: ∀ st act. String -> Props st act
cellSpacing v _ = P.cellSpacing v

charSet :: ∀ st act. String -> Props st act
charSet v _ = P.charSet v

challenge :: ∀ st act. String -> Props st act
challenge v _ = P.challenge v

checked :: ∀ st act. Boolean -> Props st act
checked v _ = P.checked v

cite :: ∀ st act. String -> Props st act
cite v _ = P.cite v

classID :: ∀ st act. String -> Props st act
classID v _ = P.classID v

className :: ∀ st act. String -> Props st act
className v _ = P.className v

cols :: ∀ st act. Int -> Props st act
cols v _ = P.cols v

colSpan :: ∀ st act. Int -> Props st act
colSpan v _ = P.colSpan v

content :: ∀ st act. String -> Props st act
content v _ = P.content v

contentEditable :: ∀ st act. Boolean -> Props st act
contentEditable v _ = P.contentEditable v

contextMenu :: ∀ st act. String -> Props st act
contextMenu v _ = P.contextMenu v

controls :: ∀ st act. Boolean -> Props st act
controls v _ = P.controls v

coords :: ∀ st act. String -> Props st act
coords v _ = P.coords v

crossOrigin :: ∀ st act. String -> Props st act
crossOrigin v _ = P.crossOrigin v

dateTime :: ∀ st act. String -> Props st act
dateTime v _ = P.dateTime v

default :: ∀ st act. Boolean -> Props st act
default v _ = P.default v

defaultChecked :: ∀ st act. Boolean -> Props st act
defaultChecked v _ = P.defaultChecked v

defaultValue :: ∀ st act. String -> Props st act
defaultValue v _ = P.defaultValue v

defer :: ∀ st act. Boolean -> Props st act
defer v _ = P.defer v

dir :: ∀ st act. String -> Props st act
dir v _ = P.dir v

disabled :: ∀ st act. Boolean -> Props st act
disabled v _ = P.disabled v

download :: ∀ st act. String -> Props st act
download v _ = P.download v

draggable :: ∀ st act. Boolean -> Props st act
draggable v _ = P.draggable v

encType :: ∀ st act. String -> Props st act
encType v _ = P.encType v

form :: ∀ st act. String -> Props st act
form v _ = P.form v

formAction :: ∀ st act. String -> Props st act
formAction v _ = P.formAction v

formEncType :: ∀ st act. String -> Props st act
formEncType v _ = P.formEncType v

formMethod :: ∀ st act. String -> Props st act
formMethod v _ = P.formMethod v

formNoValidate :: ∀ st act. Boolean -> Props st act
formNoValidate v _ = P.formNoValidate v

formTarget :: ∀ st act. String -> Props st act
formTarget v _ = P.formTarget v

frameBorder :: ∀ st act. String -> Props st act
frameBorder v _ = P.frameBorder v

headers :: ∀ st act. String -> Props st act
headers v _ = P.headers v

height :: ∀ st act. String -> Props st act
height v _ = P.height v

hidden :: ∀ st act. Boolean -> Props st act
hidden v _ = P.hidden v

high :: ∀ st act. String -> Props st act
high v _ = P.high v

href :: ∀ st act. String -> Props st act
href v _ = P.href v

hrefLang :: ∀ st act. String -> Props st act
hrefLang v _ = P.hrefLang v

htmlFor :: ∀ st act. String -> Props st act
htmlFor v _ = P.htmlFor v

httpEquiv :: ∀ st act. String -> Props st act
httpEquiv v _ = P.httpEquiv v

icon :: ∀ st act. String -> Props st act
icon v _ = P.icon v

_id :: ∀ st act. String -> Props st act
_id v _ = P._id v

inputMode :: ∀ st act. String -> Props st act
inputMode v _ = P.inputMode v

integrity :: ∀ st act. String -> Props st act
integrity v _ = P.integrity v

is :: ∀ st act. String -> Props st act
is v _ = P.is v

key :: ∀ st act. String -> Props st act
key v _ = P.key v

keyParams :: ∀ st act. String -> Props st act
keyParams v _ = P.keyParams v

keyType :: ∀ st act. String -> Props st act
keyType v _ = P.keyType v

kind :: ∀ st act. String -> Props st act
kind v _ = P.kind v

label :: ∀ st act. String -> Props st act
label v _ = P.label v

lang :: ∀ st act. String -> Props st act
lang v _ = P.lang v

list :: ∀ st act. String -> Props st act
list v _ = P.list v

loop :: ∀ st act. Boolean -> Props st act
loop v _ = P.loop v

low :: ∀ st act. String -> Props st act
low v _ = P.low v

manifest :: ∀ st act. String -> Props st act
manifest v _ = P.manifest v

marginHeight :: ∀ st act. String -> Props st act
marginHeight v _ = P.marginHeight v

marginWidth :: ∀ st act. String -> Props st act
marginWidth v _ = P.marginWidth v

max :: ∀ st act. String -> Props st act
max v _ = P.max v

maxLength :: ∀ st act. String -> Props st act
maxLength v _ = P.maxLength v

media :: ∀ st act. String -> Props st act
media v _ = P.media v

mediaGroup :: ∀ st act. String -> Props st act
mediaGroup v _ = P.mediaGroup v

method :: ∀ st act. String -> Props st act
method v _ = P.method v

min :: ∀ st act. String -> Props st act
min v _ = P.min v

minLength :: ∀ st act. String -> Props st act
minLength v _ = P.minLength v

multiple :: ∀ st act. Boolean -> Props st act
multiple v _ = P.multiple v

muted :: ∀ st act. Boolean -> Props st act
muted v _ = P.muted v

name :: ∀ st act. String -> Props st act
name v _ = P.name v

nonce :: ∀ st act. String -> Props st act
nonce v _ = P.nonce v

noValidate :: ∀ st act. Boolean -> Props st act
noValidate v _ = P.noValidate v

open :: ∀ st act. Boolean -> Props st act
open v _ = P.open v

optimum :: ∀ st act. String -> Props st act
optimum v _ = P.optimum v

pattern :: ∀ st act. String -> Props st act
pattern v _ = P.pattern v

placeholder :: ∀ st act. String -> Props st act
placeholder v _ = P.placeholder v

poster :: ∀ st act. String -> Props st act
poster v _ = P.poster v

preload :: ∀ st act. String -> Props st act
preload v _ = P.preload v

profile :: ∀ st act. String -> Props st act
profile v _ = P.profile v

radioGroup :: ∀ st act. String -> Props st act
radioGroup v _ = P.radioGroup v

readOnly :: ∀ st act. Boolean -> Props st act
readOnly v _ = P.readOnly v

rel :: ∀ st act. String -> Props st act
rel v _ = P.rel v

required :: ∀ st act. Boolean -> Props st act
required v _ = P.required v

reversed :: ∀ st act. Boolean -> Props st act
reversed v _ = P.reversed v

role :: ∀ st act. String -> Props st act
role v _ = P.role v

rows :: ∀ st act. Int -> Props st act
rows v _ = P.rows v

rowSpan :: ∀ st act. Int -> Props st act
rowSpan v _ = P.rowSpan v

sandbox :: ∀ st act. String -> Props st act
sandbox v _ = P.sandbox v

scope :: ∀ st act. String -> Props st act
scope v _ = P.scope v

scoped :: ∀ st act. Boolean -> Props st act
scoped v _ = P.scoped v

scrolling :: ∀ st act. String -> Props st act
scrolling v _ = P.scrolling v

seamless :: ∀ st act. Boolean -> Props st act
seamless v _ = P.seamless v

selected :: ∀ st act. Boolean -> Props st act
selected v _ = P.selected v

shape :: ∀ st act. String -> Props st act
shape v _ = P.shape v

size :: ∀ st act. Int -> Props st act
size v _ = P.size v

sizes :: ∀ st act. String -> Props st act
sizes v _ = P.sizes v

span :: ∀ st act. Int -> Props st act
span v _ = P.span v

spellCheck :: ∀ st act. Boolean -> Props st act
spellCheck v _ = P.spellCheck v

src :: ∀ st act. String -> Props st act
src v _ = P.src v

srcDoc :: ∀ st act. String -> Props st act
srcDoc v _ = P.srcDoc v

srcLang :: ∀ st act. String -> Props st act
srcLang v _ = P.srcLang v

srcSet :: ∀ st act. String -> Props st act
srcSet v _ = P.srcSet v

start :: ∀ st act. Int -> Props st act
start v _ = P.start v

step :: ∀ st act. String -> Props st act
step v _ = P.step v

summary :: ∀ st act. String -> Props st act
summary v _ = P.summary v

tabIndex :: ∀ st act. Int -> Props st act
tabIndex v _ = P.tabIndex v

target :: ∀ st act. String -> Props st act
target v _ = P.target v

title :: ∀ st act. String -> Props st act
title v _ = P.title v

_type :: ∀ st act. String -> Props st act
_type v _ = P._type v

useMap :: ∀ st act. String -> Props st act
useMap v _ = P.useMap v

value :: ∀ st act. String -> Props st act
value v _ = P.value v

width :: ∀ st act. String -> Props st act
width v _ = P.width v

wmode :: ∀ st act. String -> Props st act
wmode v _ = P.wmode v

wrap :: ∀ st act. String -> Props st act
wrap v _ = P.wrap v

-- RDFa Attributes
about :: ∀ st act. String -> Props st act
about v _ = P.about v

datatype :: ∀ st act. String -> Props st act
datatype v _ = P.datatype v

inlist :: ∀ st act. String -> Props st act
inlist v _ = P.inlist v

prefix :: ∀ st act. String -> Props st act
prefix v _ = P.prefix v

property :: ∀ st act. String -> Props st act
property v _ = P.property v

resource :: ∀ st act. String -> Props st act
resource v _ = P.resource v

typeof :: ∀ st act. String -> Props st act
typeof v _ = P.typeof v

vocab :: ∀ st act. String -> Props st act
vocab v _ = P.vocab v

-- Non-standard Attributes
autoCapitalize :: ∀ st act. String -> Props st act
autoCapitalize v _ = P.autoCapitalize v

autoCorrect :: ∀ st act. String -> Props st act
autoCorrect v _ = P.autoCorrect v

autoSave :: ∀ st act. String -> Props st act
autoSave v _ = P.autoSave v

color :: ∀ st act. String -> Props st act
color v _ = P.color v

itemProp :: ∀ st act. String -> Props st act
itemProp v _ = P.itemProp v

itemScope :: ∀ st act. Boolean -> Props st act
itemScope v _ = P.itemScope v

itemType :: ∀ st act. String -> Props st act
itemType v _ = P.itemType v

itemID :: ∀ st act. String -> Props st act
itemID v _ = P.itemID v

itemRef :: ∀ st act. String -> Props st act
itemRef v _ = P.itemRef v

results :: ∀ st act. Int -> Props st act
results v _ = P.results v

security :: ∀ st act. String -> Props st act
security v _ = P.security v

unselectable :: ∀ st act. Boolean -> Props st act
unselectable v _ = P.unselectable v

--------------------------------------------------------------------------------

onAnimationStart :: ∀ st act.
  (Event.SyntheticAnimationEvent -> Eff st act Unit) -> Props st act
onAnimationStart f effect = P.onAnimationStart (effect <<< f)

onAnimationEnd :: ∀ st act.
  (Event.SyntheticAnimationEvent -> Eff st act Unit) -> Props st act
onAnimationEnd f effect = P.onAnimationEnd (effect <<< f)

onAnimationIteration :: ∀ st act.
  (Event.SyntheticAnimationEvent -> Eff st act Unit) -> Props st act
onAnimationIteration f effect = P.onAnimationIteration (effect <<< f)

onTransitionEnd :: ∀ st act.
  (Event.SyntheticTransitionEvent -> Eff st act Unit) -> Props st act
onTransitionEnd f effect = P.onTransitionEnd (effect <<< f)

onLoad :: ∀ st act.
  (Event.SyntheticEvent -> Eff st act Unit) -> Props st act
onLoad f effect = P.onLoad (effect <<< f)

onCopy :: ∀ st act.
  (Event.SyntheticClipboardEvent -> Eff st act Unit) -> Props st act
onCopy f effect = P.onCopy (effect <<< f)

onCut :: ∀ st act.
  (Event.SyntheticClipboardEvent -> Eff st act Unit) -> Props st act
onCut f effect = P.onCut (effect <<< f)

onPaste :: ∀ st act.
  (Event.SyntheticClipboardEvent -> Eff st act Unit) -> Props st act
onPaste f effect = P.onPaste (effect <<< f)

onKeyDown :: ∀ st act.
  (Event.SyntheticKeyboardEvent -> Eff st act Unit) -> Props st act
onKeyDown f effect = P.onKeyDown (effect <<< f)

onKeyPress :: ∀ st act.
  (Event.SyntheticKeyboardEvent -> Eff st act Unit) -> Props st act
onKeyPress f effect = P.onKeyPress (effect <<< f)

onKeyUp :: ∀ st act.
  (Event.SyntheticKeyboardEvent -> Eff st act Unit) -> Props st act
onKeyUp f effect = P.onKeyUp (effect <<< f)

onFocus :: ∀ st act.
  (Event.SyntheticFocusEvent -> Eff st act Unit) -> Props st act
onFocus f effect = P.onFocus (effect <<< f)

onBlur :: ∀ st act.
  (Event.SyntheticFocusEvent -> Eff st act Unit) -> Props st act
onBlur f effect = P.onBlur (effect <<< f)

onChange :: ∀ st act.
  (Event.SyntheticInputEvent -> Eff st act Unit) -> Props st act
onChange f effect = P.onChange (effect <<< f)

onInput :: ∀ st act.
  (Event.SyntheticInputEvent -> Eff st act Unit) -> Props st act
onInput f effect = P.onInput (effect <<< f)

onInvalid :: ∀ st act.
  (Event.SyntheticInputEvent -> Eff st act Unit) -> Props st act
onInvalid f effect = P.onInvalid (effect <<< f)

onSubmit :: ∀ st act.
  (Event.SyntheticInputEvent -> Eff st act Unit) -> Props st act
onSubmit f effect = P.onSubmit (effect <<< f)

onClick :: ∀ st act.
  (Event.SyntheticMouseEvent -> Eff st act Unit) -> Props st act
onClick f effect = P.onClick (effect <<< f)

onDoubleClick :: ∀ st act.
  (Event.SyntheticMouseEvent -> Eff st act Unit) -> Props st act
onDoubleClick f effect = P.onDoubleClick (effect <<< f)

onDrag :: ∀ st act.
  (Event.SyntheticMouseEvent -> Eff st act Unit) -> Props st act
onDrag f effect = P.onDrag (effect <<< f)

onDragEnd :: ∀ st act.
  (Event.SyntheticMouseEvent -> Eff st act Unit) -> Props st act
onDragEnd f effect = P.onDragEnd (effect <<< f)

onDragEnter :: ∀ st act.
  (Event.SyntheticMouseEvent -> Eff st act Unit) -> Props st act
onDragEnter f effect = P.onDragEnter (effect <<< f)

onDragExit :: ∀ st act.
  (Event.SyntheticMouseEvent -> Eff st act Unit) -> Props st act
onDragExit f effect = P.onDragExit (effect <<< f)

onDragLeave :: ∀ st act.
  (Event.SyntheticMouseEvent -> Eff st act Unit) -> Props st act
onDragLeave f effect = P.onDragLeave (effect <<< f)

onDragOver :: ∀ st act.
  (Event.SyntheticMouseEvent -> Eff st act Unit) -> Props st act
onDragOver f effect = P.onDragOver (effect <<< f)

onDragStart :: ∀ st act.
  (Event.SyntheticMouseEvent -> Eff st act Unit) -> Props st act
onDragStart f effect = P.onDragStart (effect <<< f)

onDrop :: ∀ st act.
  (Event.SyntheticMouseEvent -> Eff st act Unit) -> Props st act
onDrop f effect = P.onDrop (effect <<< f)

onMouseDown :: ∀ st act.
  (Event.SyntheticMouseEvent -> Eff st act Unit) -> Props st act
onMouseDown f effect = P.onMouseDown (effect <<< f)

onMouseEnter :: ∀ st act.
  (Event.SyntheticMouseEvent -> Eff st act Unit) -> Props st act
onMouseEnter f effect = P.onMouseEnter (effect <<< f)

onMouseLeave :: ∀ st act.
  (Event.SyntheticMouseEvent -> Eff st act Unit) -> Props st act
onMouseLeave f effect = P.onMouseLeave (effect <<< f)

onMouseMove :: ∀ st act.
  (Event.SyntheticMouseEvent -> Eff st act Unit) -> Props st act
onMouseMove f effect = P.onMouseMove (effect <<< f)

onMouseOut :: ∀ st act.
  (Event.SyntheticMouseEvent -> Eff st act Unit) -> Props st act
onMouseOut f effect = P.onMouseOut (effect <<< f)

onMouseOver :: ∀ st act.
  (Event.SyntheticMouseEvent -> Eff st act Unit) -> Props st act
onMouseOver f effect = P.onMouseOver (effect <<< f)

onMouseUp :: ∀ st act.
  (Event.SyntheticMouseEvent -> Eff st act Unit) -> Props st act
onMouseUp f effect = P.onMouseUp (effect <<< f)

onTouchCancel :: ∀ st act.
  (Event.SyntheticTouchEvent -> Eff st act Unit) -> Props st act
onTouchCancel f effect = P.onTouchCancel (effect <<< f)

onTouchEnd :: ∀ st act.
  (Event.SyntheticTouchEvent -> Eff st act Unit) -> Props st act
onTouchEnd f effect = P.onTouchEnd (effect <<< f)

onTouchMove :: ∀ st act.
  (Event.SyntheticTouchEvent -> Eff st act Unit) -> Props st act
onTouchMove f effect = P.onTouchMove (effect <<< f)

onTouchStart :: ∀ st act.
  (Event.SyntheticTouchEvent -> Eff st act Unit) -> Props st act
onTouchStart f effect = P.onTouchStart (effect <<< f)

onScroll :: ∀ st act.
  (Event.SyntheticUIEvent -> Eff st act Unit) -> Props st act
onScroll f effect = P.onScroll (effect <<< f)

onWheel :: ∀ st act.
  (Event.SyntheticWheelEvent -> Eff st act Unit) -> Props st act
onWheel f effect = P.onWheel (effect <<< f)

--------------------------------------------------------------------------------

suppressContentEditableWarning :: ∀ st act. Boolean -> Props st act
suppressContentEditableWarning v _ = P.suppressContentEditableWarning v

-- SVG attributes
x :: ∀ st act. Int -> Props st act
x v _ = P.x v

y :: ∀ st act. Int -> Props st act
y v _ = P.y v

cx :: ∀ st act. Int -> Props st act
cx v _ = P.cx v

cy :: ∀ st act. Int -> Props st act
cy v _ = P.cy v

r :: ∀ st act. Int -> Props st act
r v _ = P.r v

fill :: ∀ st act. String -> Props st act
fill v _ = P.fill v

opacity :: ∀ st act. Int -> Props st act
opacity v _ = P.opacity v

fillOpacity :: ∀ st act. Int -> Props st act
fillOpacity v _ = P.fillOpacity v

stroke :: ∀ st act. String -> Props st act
stroke v _ = P.stroke v

strokeWidth :: ∀ st act. Int -> Props st act
strokeWidth v _ = P.strokeWidth v

points :: ∀ st act. String -> Props st act
points v _ = P.points v

d :: ∀ st act. String -> Props st act
d v _ = P.d v

viewBox :: ∀ st act. String -> Props st act
viewBox v _ = P.viewBox v

--------------------------------------------------------------------------------

onEnter :: ∀ st act. Eff st act Unit -> Props st act
onEnter f effect = P.onKeyDown \e -> do
  code <- Event.keyCode e
  if code == 13.0 then effect f else pure unit

onEscape :: ∀ st act. Eff st act Unit -> Props st act
onEscape f effect = P.onKeyDown \e -> do
  code <- Event.keyCode e
  if code == 27.0 then effect f else pure unit