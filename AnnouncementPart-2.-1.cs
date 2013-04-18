'From Pharo2.0a of ''18 April 2012'' [Latest update: #20560] on 22 February 2013 at 6:10:14 pm'!!SubscriptionRegistry methodsFor: 'accessing' stamp: 'StephaneDucasse 2/22/2013 18:08'!subscriptionsHandling: anAnnouncement	^ Array streamContents: [ :s|			subscriptions do: [:each| 				(each handlesAnnouncement: anAnnouncement)					ifTrue: [ s nextPut: each ]]]! !!WeakAnnouncementSubscription methodsFor: 'announcing' stamp: 'IgorStasenko 1/3/2012 12:22'!deliver: anAnnouncement	" deliver an announcement to receiver. In case of failure, it will be handled in separate process"	^ (self handlesAnnouncement: anAnnouncement ) ifTrue: [		[action cull: anAnnouncement cull: announcer] 			on: UnhandledError fork: [:ex | ex pass ]]! !!AnnouncementSubscription methodsFor: 'announcing' stamp: 'IgorStasenko 1/3/2012 12:22'!deliver: anAnnouncement	" deliver an announcement to receiver. In case of failure, it will be handled in separate process"	^ (self handlesAnnouncement: anAnnouncement ) ifTrue: [		[action cull: anAnnouncement cull: announcer] 			on: UnhandledError fork: [:ex | ex pass ]]! !